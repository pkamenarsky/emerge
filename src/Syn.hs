{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Syn where

import Control.Applicative
import Control.Concurrent hiding (yield)

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors
import Control.Monad.IO.Class
import Control.Monad.Free

import Data.IORef
import Data.Foldable (for_)

--------------------------------------------------------------------------------

data Event a = Event (IORef (Maybe a))

newEvent :: IO (Event a)
newEvent = Event <$> newIORef Nothing

--------------------------------------------------------------------------------

data SynF v next
  = Forever
  | View v next
  | forall a. IO (IO a) (a -> next)
  | forall a. Finalize (IO ()) (Syn v a) (a -> next)
  | forall a. On (Event a) (a -> next)
  | forall a. Or (Syn v a) (Syn v a) (a -> next)
  | forall a. Monoid a => And (Syn v a) (Syn v a) (a -> next)

deriving instance Functor (SynF v)

newtype Syn v a = Syn { getSyn :: Free (SynF v) a }
  deriving (Functor, Applicative, Monad)

mapView :: (u -> v) -> Syn u a -> Syn v a
mapView _ (Syn (Pure a)) = Syn $ Pure a
mapView _ (Syn (Free Forever)) = Syn $ Free Forever
mapView f (Syn (Free (View u next))) = Syn $ Free $ View (f u) (getSyn $ mapView f $ Syn next)
mapView f (Syn (Free (IO io next))) = Syn $ Free $ IO io (fmap (getSyn . mapView f . Syn) next)
mapView f (Syn (Free (Finalize fin s next))) = Syn $ Free $ Finalize fin (mapView f s) (fmap (getSyn . mapView f . Syn) next)
mapView f (Syn (Free (On e next))) = Syn $ Free $ On e (fmap (getSyn . mapView f . Syn) next)
mapView f (Syn (Free (Or a b next))) = Syn $ Free $ Or (mapView f a) (mapView f b) (fmap (getSyn . mapView f . Syn) next)
mapView f (Syn (Free (And a b next))) = Syn $ Free $ And (mapView f a) (mapView f b) (fmap (getSyn . mapView f . Syn) next)

forever :: Syn v a
forever = Syn $ liftF Forever
view :: v -> Syn v a
view v = Syn $ do
  liftF $ View v ()
  liftF Forever

-- | Fireing events from here will cause a dedlock.
unsafeNonBlockingIO :: IO a -> Syn v a
unsafeNonBlockingIO io = Syn $ liftF $ IO io id

finalize :: IO () -> Syn v a -> Syn v a
finalize fin s = Syn $ liftF $ Finalize fin s id

on :: Event a -> Syn v a
on e = Syn $ liftF $ On e id

instance Alternative (Syn v) where
  empty = forever
  a <|> b = Syn $ liftF $ Or a b id

instance Monoid a => Semigroup (Syn v a) where
  a <> b = Syn $ liftF $ And a b id

--------------------------------------------------------------------------------

data SynR v a
  = P a                  -- pure
  | V v (IO (SynR v a))  -- view
  | B (IO (SynR v a))    -- blocked
  | F [IO ()] (IO (SynR v a)) -- finalizer
  | S                    -- stop

unblock :: Monoid v => Syn v a -> IO (SynR v a)

unblock (Syn (Pure a))       = pure $ P a
unblock (Syn (Free Forever)) = pure S

unblock s@(Syn (Free (On (Event ref) next))) = readIORef ref >>= \case
  Just r  -> pure $ B $ unblock $ Syn $ next r
  Nothing -> pure $ B $ unblock s

unblock (Syn (Free (View v next))) = pure $ V v (unblock $ Syn next)

unblock (Syn (Free (IO io next))) = do
  r <- io
  unblock $ Syn $ next r

unblock (Syn (Free (Finalize fin s next))) = pure $ F [fin] (go $ unblock s)
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

really :: IO (SynR v ()) -> IO (Either [IO ()] (IO (SynR v ()), Maybe v))
really = go []
  where
    go fs s = do
      r <- s

      case r of
        V v next   -> pure $ Right (next, Just v)
        B next     -> pure $ Right (next, Nothing)
        F fs' next -> go fs' next
        S          -> pure $ Left fs
        P _        -> pure $ Left fs

run :: Monoid v => Syn v () -> (v -> IO ()) -> IO (Maybe (Event a -> a -> IO ()))
run syn showView = do
  r <- really $ unblock syn

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

          r' <- really s

          s' <- case r' of
            Left _    -> error "blocked indefinitely/finished" -- TODO
            Right r'' -> pure r''

          writeIORef ref Nothing
          pure s'

        for_ v' showView
