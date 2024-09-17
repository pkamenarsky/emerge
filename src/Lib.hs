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

data Event a

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

