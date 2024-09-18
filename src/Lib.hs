{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lib where

import Control.Concurrent
import Control.Concurrent.Chan

import Control.Monad.Free
import Control.Monad.IO.Class

import Data.IORef

data ViewId
data ViewIndex

-- events alter the structure
-- signals govern shader uniforms

data Event a = Event (IO (Maybe a))

data SynF v next
  = View v next
  | forall a. On (Event a) (a -> next)
  | forall a. Or (Syn v a) (Syn v a) (a -> next)
  | forall a. And (Syn v a) (Syn v a) (a -> next)
  | Forever

deriving instance Functor (SynF v)

newtype Syn v a = Syn { syn :: Free (SynF v) a }
  deriving (Functor, Applicative, Monad)

forever :: Syn v a
forever = Syn $ liftF Forever

view :: v -> Syn v a
view v = Syn $ do
  liftF $ View v ()
  liftF Forever

on :: Event a -> Syn v a
on e = Syn $ liftF $ On e id

or :: Syn v a -> Syn v a -> Syn v a
or a b = Syn $ liftF $ Or a b id

and :: Syn v a -> Syn v a -> Syn v a
and a b = Syn $ liftF $ And a b id

--------------------------------------------------------------------------------

data SynR v a
  = P a            -- pure
  | V v (Syn v a)  -- view
  | B (Syn v a)    -- blocked
  | S              -- stop

unblock :: Monoid v => Syn v a -> IO (SynR v a)
unblock (Syn (Pure a)) = pure $ P a
unblock s@(Syn (Free (On (Event ref) next))) = do
  a <- ref
  case a of
    Nothing -> pure $ B s
    Just a' -> unblock $ Syn $ next a'
unblock (Syn (Free (Or a b next))) = do
  a' <- unblock a
  b' <- unblock b
  case (a', b') of
    (P r, _) -> unblock $ do
      _ <- view mempty
      Syn $ next r
    (_, P r) -> unblock $ do
      _ <- view mempty
      Syn $ next r
    (V av' an, V bv' bn) -> pure $ V (av' <> bv') (Syn $ Free $ Or an bn next)
    (V av' an, B bn)   -> pure $ V (av' <> undefined) (Syn $ Free $ Or an bn next)
      
