{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Syn.Run where

import Control.Applicative
import Control.Concurrent hiding (yield)

import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.IORef
import Data.Foldable (for_)

import qualified Syn as Syn

--------------------------------------------------------------------------------

data Event a = Event { eventRef :: IORef (Maybe a) }

newEvent :: IO (Event a)
newEvent = Event <$> newIORef Nothing

--------------------------------------------------------------------------------

newtype Syn v m a = Syn { unSyn :: Syn.Syn Event v m a }
  deriving (Functor, Applicative, Monad, Alternative, Semigroup, MonadTrans)

mapView :: (u -> v) -> Syn u m a -> Syn v m a
mapView f (Syn syn) = Syn $ Syn.mapView f syn

forever :: Syn v m a
forever = Syn $ Syn.forever

view :: v -> Syn v m a
view = Syn . Syn.view

finalize :: m () -> Syn v m a -> Syn v m a
finalize fin (Syn syn) = Syn $ Syn.finalize fin syn

on :: Event a -> Syn v m a
on = Syn . Syn.on

-- | Fireing events from here will cause a dedlock.
unsafeNonBlockingIO :: MonadIO m => IO a -> Syn v m a
unsafeNonBlockingIO = Syn . Syn.unsafeNonBlockingIO

--------------------------------------------------------------------------------

run :: Monoid v => Syn v IO () -> (v -> IO ()) -> IO (Maybe (Event a -> a -> IO ()))
run (Syn syn) showView = do
  r <- Syn.unblockAll $ Syn.unblock (readIORef . eventRef) syn

  case r of
    Left fs -> do
      sequence_ fs
      pure Nothing
    Right (next, v) -> do
      for_ v showView

      mvSyn <- newMVar next

      pure $ Just $ \(Event ref) a -> do
        v' <- modifyMVar mvSyn $ \s -> do
          writeIORef ref (Just a)

          r' <- Syn.unblockAll s

          s' <- case r' of
            Left _    -> error "blocked indefinitely/finished" -- TODO
            Right r'' -> pure r''

          writeIORef ref Nothing
          pure s'

        for_ v' showView
