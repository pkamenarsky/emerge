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
import Data.Void

--------------------------------------------------------------------------------

data SynF v m next
  = Forever
  | View v next
  | Blocked next
  | forall a. Lift (m a) (a -> next)
  | forall a. Finalize (m ()) (Syn v m a) (a -> next)
  | forall a. Or (Syn v m a) (Syn v m a) (a -> next)
  | forall a. Monoid a => And (Syn v m a) (Syn v m a) (a -> next)

deriving instance Functor (SynF v m)

newtype Syn v m a = Syn { unSyn :: Free (SynF v m) a }
  deriving (Functor, Applicative, Monad)

instance Alternative (Syn v m) where
  empty = forever
  a <|> b = Syn $ liftF $ Or a b id

instance Monoid a => Semigroup (Syn v m a) where
  a <> b = Syn $ liftF $ And a b id

instance MonadTrans (Syn v) where
  lift m = Syn $ liftF $ Lift m id

mapView :: (u -> v) -> Syn u m a -> Syn v m a
mapView _ (Syn (Pure a)) = Syn $ Pure a
mapView _ (Syn (Free Forever)) = Syn $ Free Forever
mapView f (Syn (Free (View u next))) = Syn $ Free $ View (f u) (unSyn $ mapView f $ Syn next)
mapView f (Syn (Free (Lift m next))) = Syn $ Free $ Lift m (fmap (unSyn . mapView f . Syn) next)
mapView f (Syn (Free (Finalize fin s next))) = Syn $ Free $ Finalize fin (mapView f s) (fmap (unSyn . mapView f . Syn) next)
mapView f (Syn (Free (Blocked next))) = Syn $ Free $ Blocked (unSyn $ mapView f $ Syn next)
mapView f (Syn (Free (Or a b next))) = Syn $ Free $ Or (mapView f a) (mapView f b) (fmap (unSyn . mapView f . Syn) next)
mapView f (Syn (Free (And a b next))) = Syn $ Free $ And (mapView f a) (mapView f b) (fmap (unSyn . mapView f . Syn) next)

forever :: Syn v m a
forever = Syn $ liftF Forever

view :: v -> Syn v m a
view v = Syn $ do
  liftF $ View v ()
  liftF Forever

finalize :: m () -> Syn v m a -> Syn v m a
finalize fin s = Syn $ liftF $ Finalize fin s id

-- | Fireing events from here will cause a dedlock.
unsafeNonBlockingIO :: MonadIO m => IO a -> Syn v m a
unsafeNonBlockingIO io = lift $ liftIO io

--------------------------------------------------------------------------------

data SynR v m a
  = P a                       -- pure
  | V v (m (SynR v m a))      -- view
  | B (m (SynR v m a))        -- blocked
  | F [m ()] (m (SynR v m a)) -- finalizer
  | S                         -- stop

unblock :: Monad m => Monoid v => Syn v m a -> m (SynR v m a)

unblock (Syn (Pure a))       = pure $ P a
unblock (Syn (Free Forever)) = pure S

unblock (Syn (Free (Blocked next))) = pure $ B $ unblock $ Syn next

unblock (Syn (Free (View v next))) = pure $ V v $ unblock $ Syn next

unblock (Syn (Free (Lift m next))) = do
  r <- m
  unblock $ Syn $ next r

unblock (Syn (Free (Finalize fin s next))) = pure $ F [fin] $ go $ unblock s
  where
    go syn = do
      r <- syn

      case r of
        P a -> do
          fin
          pure $ F [] $ unblock $ Syn $ next a
        V v next'  -> pure $ V v (go next')
        B next'    -> pure $ B (go next')
        F fs next' -> pure $ F (fin:fs) (go next')
        S          -> pure S

unblock (Syn (Free (Or a b next))) = go [] [] (unblock a) (unblock b) mempty mempty
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
          pure $ F [] $ unblock $ Syn $ next r
        (_, P r) -> do
          sequence_ aFins
          pure $ F [] $ unblock $ Syn $ next r

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

unblock (Syn (Free (And a b next))) = go [] [] (unblock a) (unblock b) mempty mempty
  where
    go aFins bFins aSyn bSyn aPrV bPrV = do
      a' <- aSyn
      b' <- bSyn

      case (a', b') of
        -- finalizers
        (F fs next', s) -> pure $ F (fs <> bFins) $ go fs bFins next' (pure s) aPrV bPrV
        (s, F fs next') -> pure $ F (aFins <> fs) $ go aFins fs (pure s) next' aPrV bPrV

        -- both finished
        (P aR, P bR) -> unblock $ Syn $ next (aR <> bR)

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

--------------------------------------------------------------------------------

data YF v m next
  = YV v next
  | YB next
  | YF [m ()] next
  | YS

deriving instance Functor (YF v m)

newtype Y v m a = Y { unY :: Free (YF v m) a }
  deriving (Functor, Applicative, Monad)

reinterpret :: Monad m => Monoid v => Syn v m a -> Y v m a
reinterpret = undefined

--------------------------------------------------------------------------------

data AF m next
  = AB next
  | AF [m ()] next
  | AS

deriving instance Functor (AF m)

newtype A m a = A { unA :: Free (AF m) a }
  deriving (Functor, Applicative, Monad)

data SynA m a b = SynA { unSynA :: a -> A m (b, SynA m a b) }

toArr :: Applicative m => Y v m Void -> SynA m () v
toArr (Y (Pure _)) = undefined -- unreachable
toArr (Y (Free (YV v next))) = SynA $ \() -> pure (v, toArr $ Y next)
toArr (Y (Free (YB next))) = SynA $ \() -> A $ Free $ AB $ unA $ unSynA (toArr $ Y next) ()
toArr (Y (Free (YF fins next))) = SynA $ \() -> A $ Free $ AF fins $ unA $ unSynA (toArr $ Y next) ()
toArr (Y (Free YS)) = SynA $ \() -> A $ Free $ AS
