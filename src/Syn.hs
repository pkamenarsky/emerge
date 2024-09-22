{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Syn where

import Control.Applicative
import Control.Concurrent hiding (yield)

import Control.Monad.IO.Class
import Control.Monad.Free
import Control.Monad.Trans.Class

import Data.IORef
import Data.Foldable (for_)

--------------------------------------------------------------------------------

data SynF e v m next
  = Forever
  | View v next
  | forall a. Lift (m a) (a -> next)
  | forall a n. Monad n => Hoist (forall b. n b -> m b) (forall b. m b -> n b) (Syn e v n a) (a -> next)
  | forall a. Finalize (m ()) (Syn e v m a) (a -> next)
  | forall a. On (e a) (a -> next)
  | forall a. Or (Syn e v m a) (Syn e v m a) (a -> next)
  | forall a. Monoid a => And (Syn e v m a) (Syn e v m a) (a -> next)

deriving instance Functor (SynF e v m)

newtype Syn e v m a = Syn { unSyn :: Free (SynF e v m) a }
  deriving (Functor, Applicative, Monad)

instance Alternative (Syn e v m) where
  empty = forever
  a <|> b = Syn $ liftF $ Or a b id

instance Monoid a => Semigroup (Syn e v m a) where
  a <> b = Syn $ liftF $ And a b id

instance MonadTrans (Syn e v) where
  lift m = Syn $ liftF $ Lift m id

mapView :: (u -> v) -> Syn e u m a -> Syn e v m a
mapView _ (Syn (Pure a)) = Syn $ Pure a
mapView _ (Syn (Free Forever)) = Syn $ Free Forever
mapView f (Syn (Free (View u next))) = Syn $ Free $ View (f u) (unSyn $ mapView f $ Syn next)
mapView f (Syn (Free (Lift m next))) = Syn $ Free $ Lift m (fmap (unSyn . mapView f . Syn) next)
mapView f (Syn (Free (Hoist m n s next))) = Syn $ Free $ Hoist m n (mapView f s) (fmap (unSyn . mapView f . Syn) next)
mapView f (Syn (Free (Finalize fin s next))) = Syn $ Free $ Finalize fin (mapView f s) (fmap (unSyn . mapView f . Syn) next)
mapView f (Syn (Free (On e next))) = Syn $ Free $ On e (fmap (unSyn . mapView f . Syn) next)
mapView f (Syn (Free (Or a b next))) = Syn $ Free $ Or (mapView f a) (mapView f b) (fmap (unSyn . mapView f . Syn) next)
mapView f (Syn (Free (And a b next))) = Syn $ Free $ And (mapView f a) (mapView f b) (fmap (unSyn . mapView f . Syn) next)

hoist :: Monad m => (forall b. m b -> n b) -> (forall b. n b -> m b) -> Syn e v m a -> Syn e v n a
hoist f g s = Syn $ liftF $ Hoist f g s id

forever :: Syn e v m a
forever = Syn $ liftF Forever

view :: v -> Syn e v m a
view v = Syn $ do
  liftF $ View v ()
  liftF Forever

finalize :: m () -> Syn e v m a -> Syn e v m a
finalize fin s = Syn $ liftF $ Finalize fin s id

on :: e a -> Syn e v m a
on e = Syn $ liftF $ On e id

-- | Fireing events from here will cause a dedlock.
unsafeNonBlockingIO :: MonadIO m => IO a -> Syn e v m a
unsafeNonBlockingIO io = lift $ liftIO io

--------------------------------------------------------------------------------

data SynR v m a
  = P a                       -- pure
  | V v (m (SynR v m a))      -- view
  | B (m (SynR v m a))        -- blocked
  | F [m ()] (m (SynR v m a)) -- finalizer
  | S                         -- stop

unblock :: Monad m => Monoid v => (forall b. e b -> m (Maybe b)) -> Syn e v m a -> m (SynR v m a)

unblock _ (Syn (Pure a))       = pure $ P a
unblock _ (Syn (Free Forever)) = pure S

unblock re s@(Syn (Free (On e next))) = re e >>= \case
  Just r  -> pure $ B $ unblock re $ Syn $ next r
  Nothing -> pure $ B $ unblock re s

unblock re (Syn (Free (View v next))) = pure $ V v (unblock re $ Syn next)

unblock re (Syn (Free (Lift m next))) = do
  r <- m
  unblock re $ Syn $ next r

unblock re (Syn (Free (Hoist h g s next))) = do
  r <- h go0

  case r of
    P a        -> unblock undefined $ Syn $ next a
    V v next'  -> pure $ _ $ V v (go next')

  where
    go0 = go $ unblock (fmap g re) s

    go syn = do
      r <- syn

      case r of
        P a        -> pure $ P a
        V v next'  -> pure $ V v (go next')
        B next'    -> pure $ B (go next')
        F fs next' -> pure $ F fs (go next')
        S          -> pure S

unblock re (Syn (Free (Finalize fin s next))) = pure $ F [fin] (go $ unblock re s)
  where
    go syn = do
      r <- syn

      case r of
        P a -> do
          fin
          pure $ F [] $ unblock re $ Syn $ next a
        V v next'  -> pure $ V v (go next')
        B next'    -> pure $ B (go next')
        F fs next' -> pure $ F (fin:fs) (go next')
        S          -> pure S

unblock re (Syn (Free (Or a b next))) = go [] [] (unblock re a) (unblock re b) mempty mempty
  where
    go aFins bFins aSyn bSyn aPrV bPrV = do
      a' <- aSyn
      b' <- bSyn

      case (a', b') of
        -- finalizers
        (F fs next', s) -> pure $ F (fs <> bFins) $ go fs bFins next' (pure s) aPrV bPrV
        (s, F fs next') -> pure $ F (aFins <> fs) $ go aFins fs (pure s) next' aPrV bPrV

        -- one finished
        (P r, _) -> do
          sequence_ bFins
          pure $ F [] $ unblock re $ Syn $ next r
        (_, P r) -> do
          sequence_ aFins
          pure $ F [] $ unblock re $ Syn $ next r

        -- both views
        (V aV aNext, V bV bNext) -> pure $ V (aV <> bV) (go aFins bFins aNext bNext aV bV)

        -- left view, remaining variants
        (V aV aNext, B bNext) -> pure $ V (aV <> bPrV) (go aFins bFins aNext bNext aV bPrV)
        (V aV aNext, S)       -> pure $ V (aV <> bPrV) (go aFins bFins aNext (pure S) aV bPrV)

        -- right view, remaining variants
        (B aNext, V bV bNext)  -> pure $ V (aPrV <> bV) (go aFins bFins aNext bNext aPrV bV)
        (S, V bV bNext)        -> pure $ V (aPrV <> bV) (go aFins bFins (pure S) bNext aPrV bV)

        -- both blocked
        (B aNext, B bNext) -> pure $ B $ go aFins bFins aNext bNext aPrV bPrV

        -- both stopped
        (S, S) -> pure S

        -- one stopped
        (S, B bNext) -> pure $ B $ go aFins bFins (pure S) bNext aPrV bPrV
        (B aNext, S) -> pure $ B $ go aFins bFins aNext (pure S) aPrV bPrV

unblock re (Syn (Free (And a b next))) = go [] [] (unblock re a) (unblock re b) mempty mempty
  where
    go aFins bFins aSyn bSyn aPrV bPrV = do
      a' <- aSyn
      b' <- bSyn

      case (a', b') of
        -- finalizers
        (F fs next', s) -> pure $ F (fs <> bFins) $ go fs bFins next' (pure s) aPrV bPrV
        (s, F fs next') -> pure $ F (aFins <> fs) $ go aFins fs (pure s) next' aPrV bPrV

        -- both finished
        (P aR, P bR) -> unblock re $ Syn $ next (aR <> bR)

        -- both views
        (V aV aNext, V bV bNext) -> pure $ V (aV <> bV) (go aFins bFins aNext bNext aV bV)

        -- left view, remaining variants
        (V aV aNext, B bNext) -> pure $ V (aV <> bPrV) (go aFins bFins aNext bNext aV bPrV)
        (V aV aNext, s)       -> pure $ V (aV <> bPrV) (go aFins bFins aNext (pure s) aV bPrV)

        -- right view, remaining variants
        (B aNext, V bV bNext)  -> pure $ V (aPrV <> bV) (go aFins bFins aNext bNext aPrV bV)
        (s, V bV bNext)        -> pure $ V (aPrV <> bV) (go aFins bFins (pure s) bNext aPrV bV)

        -- both blocked
        (B aNext, B bNext) -> pure $ B $ go aFins bFins aNext bNext aPrV bPrV

        -- one blocked, the other finished
        (B aNext, p@(P _)) -> pure $ B $ go aFins bFins aNext (pure p) aPrV bPrV
        (p@(P _), B bNext) -> pure $ B $ go aFins bFins (pure p) bNext aPrV bPrV

        -- stopped
        (S, _) -> pure S
        (_, S) -> pure S

--------------------------------------------------------------------------------

unblockAll :: Monad m => m (SynR v m ()) -> m (Either [m ()] (m (SynR v m ()), Maybe v))
unblockAll = go []
  where
    go fs s = do
      r <- s

      case r of
        V v next   -> pure $ Right (next, Just v)
        B next     -> pure $ Right (next, Nothing)
        F fs' next -> go fs' next
        S          -> pure $ Left fs
        P _        -> pure $ Left fs
