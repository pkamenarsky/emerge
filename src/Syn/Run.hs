{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Syn.Run where

import Control.Concurrent hiding (yield)

import Control.Monad.Free
import Control.Monad.IO.Class

import Data.IORef
import Data.Foldable (for_)
import Data.Void

import Syn

--------------------------------------------------------------------------------

data Event a = Event (IORef (Maybe a))

newEvent :: IO (Event a)
newEvent = Event <$> newIORef Nothing

on :: MonadIO m => Event a -> Syn v m a
on e@(Event ref) = unsafeNonBlockingIO (readIORef ref) >>= \case
  Nothing -> Syn $ Free $ Blocked $ unSyn $ on e

  -- block here, otherwise something like `on e >> on e >> on e` will run to
  -- completion in one go; see [0]
  Just a  -> Syn $ Free $ Blocked $ pure a

--------------------------------------------------------------------------------

run :: Monoid v => Syn v IO Void -> (v -> IO ()) -> IO (Maybe (Event a -> a -> IO ()))
run syn showView = do
  (next, v) <- unblock $ reinterpret syn

  for_ v showView

  mvSyn <- newMVar next

  pure $ Just $ \(Event ref) a -> do
    v' <- modifyMVar mvSyn $ \s -> do

      writeIORef ref (Just a)
      (next', v') <- unblock s
      writeIORef ref Nothing

      case v' of
        -- [0]: blocked; unblock potential `Blocked (Pure _)` expressions
        Nothing -> unblock next'
        Just _  -> pure (next', v')

    for_ v' showView
