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
  = View v next
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
mapView f (Syn (Free (View u next))) = Syn $ Free $ View (f u) (unSyn $ mapView f $ Syn next)
mapView f (Syn (Free (Lift m next))) = Syn $ Free $ Lift m (fmap (unSyn . mapView f . Syn) next)
mapView f (Syn (Free (Finalize fin s next))) = Syn $ Free $ Finalize fin (mapView f s) (fmap (unSyn . mapView f . Syn) next)
mapView f (Syn (Free (Blocked next))) = Syn $ Free $ Blocked (unSyn $ mapView f $ Syn next)
mapView f (Syn (Free (Or a b next))) = Syn $ Free $ Or (mapView f a) (mapView f b) (fmap (unSyn . mapView f . Syn) next)
mapView f (Syn (Free (And a b next))) = Syn $ Free $ And (mapView f a) (mapView f b) (fmap (unSyn . mapView f . Syn) next)

forever :: Syn v m a
forever = Syn $ Free $ Blocked $ unSyn forever

view :: v -> Syn v m a
view v = Syn $ Free $ View v $ unSyn forever

finalize :: m () -> Syn v m a -> Syn v m a
finalize fin s = Syn $ liftF $ Finalize fin s id

-- | Fireing events from here will cause a dedlock.
unsafeNonBlockingIO :: MonadIO m => IO a -> Syn v m a
unsafeNonBlockingIO io = lift $ liftIO io

--------------------------------------------------------------------------------

data YF v m next
  = YV v next
  | forall a. YA (m a) (a -> next)
  | YB next
  | YF [m ()] next

deriving instance Functor (YF v m)

newtype Y v m a = Y { unY :: Free (YF v m) a }
  deriving (Functor, Applicative, Monad)

yv :: v -> Y v m ()
yv v = Y $ liftF $ YV v ()

ya :: m a -> Y v m a
ya act = Y $ liftF $ YA act id

yb :: Y v m ()
yb = Y $ liftF $ YB ()

yf :: [m ()] -> Y v m ()
yf fins = Y $ liftF $ YF fins ()

reinterpret :: Monad m => Monoid v => Syn v m a -> Y v m a

reinterpret (Syn (Pure a)) = pure a

reinterpret (Syn (Free (View v next)))   = yv v >> reinterpret (Syn next)
reinterpret (Syn (Free (Blocked next)))  = yb >> reinterpret (Syn next)
reinterpret (Syn (Free (Lift act next))) = ya act >>= reinterpret . Syn . next

reinterpret (Syn (Free (Finalize fin syn next))) = do
  yf [fin]
  go (unY $ reinterpret syn)
  where
    go y = do
      case y of
        Pure a -> do
          ya fin
          yf []
          reinterpret (Syn $ next a)
        Free (YV v next')  -> yv v >> go next'
        Free (YA a next')  -> ya a >>= go . next'
        Free (YB next')    -> yb >> go next'
        Free (YF fs next') -> yf (fin:fs) >> go next'

reinterpret (Syn (Free (Or a b next))) = go [] [] (unY $ reinterpret a) (unY $ reinterpret b) mempty mempty
  where
    go aFins bFins aY bY aPrV bPrV = case (aY, bY) of
      -- finalizers
      (Free (YF fs next'), x) -> yf (fs <> bFins) >> go fs bFins next' x aPrV bPrV
      (x, Free (YF fs next')) -> yf (aFins <> fs) >> go aFins fs x next' aPrV bPrV

      -- actions
      (Free (YA act aNext), x) -> ya act >>= \a' -> go aFins bFins (aNext a') x aPrV bPrV
      (x, Free (YA act bNext)) -> ya act >>= \a' -> go aFins bFins x (bNext a') aPrV bPrV

      -- one finished
      (Pure r, _) -> do
        ya $ sequence_ bFins
        yf []
        reinterpret (Syn $ next r)
      (_, Pure r) -> do
        ya $ sequence_ aFins
        yf []
        reinterpret (Syn $ next r)

      -- both views
      (Free (YV aV aNext), Free (YV bV bNext)) -> yv (aV <> bV) >> go aFins bFins aNext bNext aV bV

      -- one view
      (Free (YV aV aNext), Free (YB bNext)) -> yv (aV <> bPrV) >> go aFins bFins aNext bNext aV bPrV
      (Free (YB aNext), Free (YV bV bNext)) -> yv (aPrV <> bV) >> go aFins bFins aNext bNext aPrV bV

      -- both blocked
      (Free (YB aNext), Free (YB bNext)) -> yb >> go aFins bFins aNext bNext aPrV bPrV

reinterpret (Syn (Free (And a b next))) = go [] [] (unY $ reinterpret a) (unY $ reinterpret b) mempty mempty
  where
    go aFins bFins aY bY aPrV bPrV = case (aY, bY) of
      -- finalizers
      (Free (YF fs next'), x) -> yf (fs <> bFins) >> go fs bFins next' x aPrV bPrV
      (x, Free (YF fs next')) -> yf (aFins <> fs) >> go aFins fs x next' aPrV bPrV

      -- actions
      (Free (YA act aNext), x) -> ya act >>= \a' -> go aFins bFins (aNext a') x aPrV bPrV
      (x, Free (YA act bNext)) -> ya act >>= \a' -> go aFins bFins x (bNext a') aPrV bPrV

      -- both finished
      (Pure aR, Pure bR) -> reinterpret $ Syn $ next (aR <> bR)

      -- both views
      (Free (YV aV aNext), Free (YV bV bNext)) -> yv (aV <> bV) >> go aFins bFins aNext bNext aV bV

      -- one view
      (Free (YV aV aNext), Free (YB bNext)) -> yv (aV <> bPrV) >> go aFins bFins aNext bNext aV bPrV
      (Free (YV aV aNext), x@(Pure _))      -> yv (aV <> bPrV) >> go aFins bFins aNext x aV bPrV

      (Free (YB aNext), Free (YV bV bNext)) -> yv (aPrV <> bV) >> go aFins bFins aNext bNext aPrV bV
      (x@(Pure _), Free (YV bV bNext))      -> yv (aPrV <> bV) >> go aFins bFins x bNext aPrV bV

      -- both blocked
      (Free (YB aNext), Free (YB bNext)) -> yb >> go aFins bFins aNext bNext aPrV bPrV

      -- one blocked, the other finished
      (Free (YB aNext), x@(Pure _)) -> yb >> go aFins bFins aNext x aPrV bPrV
      (x@(Pure _), Free (YB bNext)) -> yb >> go aFins bFins x bNext aPrV bPrV

unblock :: Monad m => Y v m Void -> m (Y v m Void, Maybe v)
unblock = go [] . unY
  where
    go _fs y = case y of
        Pure _             -> undefined -- unreachable
        Free (YV v next)   -> pure (Y next, Just v)
        Free (YB next)     -> pure (Y next, Nothing)
        Free (YA act next) -> act >>= go _fs . next
        Free (YF fs next)  -> go fs next

--------------------------------------------------------------------------------

data AF m next = AB next | forall a. AA (m a) (a -> next) | AF [m ()] next

deriving instance Functor (AF m)

newtype A m a = A { unA :: Free (AF m) a }
  deriving (Functor, Applicative, Monad)

data SynA m a b = SynA { unSynA :: a -> A m (b, SynA m a b) }

toArr :: Applicative m => Y v m Void -> SynA m () v
toArr (Y (Pure _)) = undefined -- unreachable
toArr (Y (Free (YV v next))) = SynA $ \() -> pure (v, toArr $ Y next)
toArr (Y (Free (YA act next))) = SynA $ \() -> A $ Free $ AA act $ fmap (unA . ($ ()) . unSynA . toArr . Y) next
toArr (Y (Free (YB next))) = SynA $ \() -> A $ Free $ AB $ unA $ unSynA (toArr $ Y next) ()
toArr (Y (Free (YF fins next))) = SynA $ \() -> A $ Free $ AF fins $ unA $ unSynA (toArr $ Y next) ()

fromArr :: Monad m => SynA m () v -> Syn v m a
fromArr synA = case unA $ unSynA synA () of
  Pure (v, next) -> Syn $ Free $ View v $ unSyn $ fromArr next
  Free (AB next) -> Syn $ Free $ Blocked $ unSyn $ fromArr $ SynA $ \() -> A next
  Free (AF fin next) -> Syn $ Free $ Finalize (sequence_ fin) (fromArr $ SynA $ \() -> A next) (\_ -> unSyn $ fromArr $ SynA $ \() -> A next)
  Free (AA act next) -> Syn $ Free $ Lift act $ fmap (unSyn . fromArr . SynA . const . A) next
