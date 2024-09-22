{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module SynT where

import Control.Applicative
import Control.Concurrent hiding (yield)

import Control.Monad.IO.Class
import Control.Monad.Free

import Data.IORef
import Data.Foldable (for_)

--------------------------------------------------------------------------------

data Event a = Event (Chan a)

newEvent :: IO (Event a)
newEvent = Event <$> newChan

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

data Y v a
  = P a  -- pure
  | V v  -- view
  | B    -- blocked
  | S    -- stop

unblock :: Monoid v => MVar (Y v a) -> Syn v a -> IO ()

unblock y (Syn (Pure a))       = putMVar y (P a) >> pure ()
unblock y (Syn (Free Forever)) = putMVar y S

unblock y (Syn (Free (On (Event bc) next))) = do
  c <- dupChan bc
  putMVar y B
  a <- readChan c
  unblock y $ Syn $ next a

unblock y (Syn (Free (View v next))) = do
  putMVar y (V v)
  unblock y $ Syn next

unblock y (Syn (Free (IO io next))) = do
  r <- io
  unblock y $ Syn $ next r

-- unblock (Syn (Free (Finalize fin s next))) = pure $ F fin (unblock s) (fmap Syn next)

-- --unblock (Syn (Free (Finalize fin s next))) = go (unblock s)
-- --  where
-- --    go syn = do
-- --      r <- syn
-- --      case r of
-- --        P a -> do
-- --          fin
-- --          unblock $ Syn $ next a
-- --        V v next' -> pure $ V v (go next')
-- --        B next'   -> pure $ B (go next')
-- --        S         -> pure S
-- 
unblock y (Syn (Free (Or a b next))) = do
  aM <- newEmptyMVar
  bM <- newEmptyMVar

  atid <- forkIO (unblock aM a)
  btid <- forkIO (unblock bM b)

  go atid btid (Just aM) (Just bM) mempty mempty

  where
    go atid btid aM bM aPrV bPrV = do
      a' <- maybe (pure S) takeMVar aM
      b' <- maybe (pure S) takeMVar bM

      case (a', b') of
        -- one finished
        (P r, _) -> do
          killThread btid
          unblock y $ Syn $ next r
        (_, P r) -> do
          killThread atid
          unblock y $ Syn $ next r

        -- both views
        (V aV, V bV) -> do
          putMVar y $ V (aV <> bV)
          go atid btid aM bM aV bV

        -- left view, remaining variants
        (V aV, B) -> do
          putMVar y $ V (aV <> bPrV)
          putMVar y B
          go atid btid aM bM aV bPrV

        (V aV, _) -> do
          putMVar y $ V (aV <> bPrV)
          go atid btid aM bM aV bPrV

        -- right view, remaining variants
        (B, V bV)  -> do
          putMVar y $ V (aPrV <> bV)
          putMVar y B
          go atid btid aM bM aPrV bV

        (_, V bV)  -> do
          putMVar y $ V (aPrV <> bV)
          go atid btid aM bM aPrV bV

        -- both blocked
        (B, B) -> do
          putMVar y B
          go atid btid aM bM aPrV bPrV

        -- both stopped
        (S, S) -> putMVar y S

        -- one stopped
        (S, B) -> do
          putMVar y B
          go atid btid Nothing bM aPrV bPrV
        (B, S) -> do
          putMVar y B
          go atid btid aM Nothing aPrV bPrV

unblock y (Syn (Free (And a b next))) = do
  aM <- newEmptyMVar
  bM <- newEmptyMVar

  _ <- forkIO (unblock aM a)
  _ <- forkIO (unblock bM b)

  go (Right aM) (Right bM) mempty mempty

  where
    go aM bM aPrV bPrV = do
      a' <- either (pure . P) takeMVar aM
      b' <- either (pure . P) takeMVar bM

      case (a', b') of
        -- both finished
        (P aR, P bR) -> unblock y $ Syn $ next (aR <> bR)

        -- both views
        (V aV, V bV) -> do
          putMVar y $ V (aV <> bV)
          go aM bM aV bV

        -- left view, remaining variants
        (V aV, B) -> do
          putMVar y $ V (aV <> bPrV)
          putMVar y B
          go aM bM aV bPrV

        (V aV, _) -> do
          putMVar y $ V (aV <> bPrV)
          go aM bM aV bPrV

        -- right view, remaining variants
        (B, V bV)  -> do
          putMVar y $ V (aPrV <> bV)
          putMVar y B
          go aM bM aPrV bV

        (_, V bV)  -> do
          putMVar y $ V (aPrV <> bV)
          go aM bM aPrV bV

        -- both blocked
        (B, B) -> do
          putMVar y B
          go aM bM aPrV bPrV

        -- one blocked, the other finished
        (B, P b'') -> do
          putMVar y B
          go aM (Left b'') aPrV bPrV

        (P a'', B) -> do
          putMVar y B
          go (Left a'') bM aPrV bPrV

        -- stopped
        (S, _) -> putMVar y S >> threadDelay maxBound
        (_, S) -> putMVar y S >> threadDelay maxBound

--------------------------------------------------------------------------------

run :: Monoid v => Syn v a -> (v -> IO ()) -> IO (Event a -> a -> IO (), IO (Maybe a))
run syn showView = do
  y <- newEmptyMVar
  e <- newEmptyMVar

  _ <- forkIO $ unblock y syn

  pure (\(Event c) a -> takeMVar e >> writeChan c a, go e y)
  where
    -- TODO: make exception safe
    go e y = do
      r <- takeMVar y

      case r of
        P a -> pure $ Just a
        V v -> showView v >> go e y
        B   -> putMVar e () >> go e y
        S   -> pure Nothing
