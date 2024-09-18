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
  | forall a. Monoid a => And (Syn v a) (Syn v a) (a -> next)
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

and :: Monoid a => Syn v a -> Syn v a -> Syn v a
and a b = Syn $ liftF $ And a b id

--------------------------------------------------------------------------------

data SynR v a
  = P a                  -- pure
  | V v (IO (SynR v a))  -- view
  | B (IO (SynR v a))    -- blocked
  | S                    -- stop

unblock :: Monoid v => Syn v a -> IO (SynR v a)
unblock (Syn (Pure a)) = pure $ P a
unblock s@(Syn (Free (On (Event ref) next))) = do
  a <- ref
  case a of
    Nothing -> pure $ B $ unblock s
    Just a' -> unblock $ Syn $ next a'
unblock (Syn (Free (View v next))) = pure $ V v (unblock $ Syn next)
unblock (Syn (Free Forever)) = pure S
unblock s@(Syn (Free (Or a b next))) = go (unblock a) (unblock b) mempty mempty
  where
    go aSyn bSyn aPrV bPrV = do
      a' <- aSyn
      b' <- bSyn

      case (a', b') of
        -- one finished
        (P r, _) -> pure $ V mempty (unblock $ Syn $ next r)
        (_, P r) -> pure $ V mempty (unblock $ Syn $ next r)

        -- both views
        (V aV aNext, V bV bNext) -> pure $ V (aV <> bV) (go aNext bNext aV bV)

        -- left view, remaining variants
        (V aV aNext, B bNext) -> pure $ V (aV <> bPrV) (go aNext bNext aV bPrV)
        (V aV aNext, S)       -> pure $ V (aV <> bPrV) (go aNext (pure S) aV bPrV)

        -- right view, remaining variants
        (B aNext, V bV bNext)  -> pure $ V (aPrV <> bV) (go aNext bNext aPrV bV)
        (S, V bV bNext)        -> pure $ V (aPrV <> bV) (go (pure S) bNext aPrV bV)

        -- both blocked
        (B aNext, B bNext) -> pure $ B $ go aNext bNext aPrV bPrV

        -- both stopped
        (S, S)             -> pure S

        -- one stopped
        (S, _)             -> pure $ B $ unblock s
        (_, S)             -> pure $ B $ unblock s
