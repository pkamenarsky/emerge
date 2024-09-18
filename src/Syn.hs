{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Syn where

import Control.Applicative
import Control.Concurrent

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
mapView f (Syn (Free (On e next))) = Syn $ Free $ On e (fmap (getSyn . mapView f . Syn) next)
mapView f (Syn (Free (Or a b next))) = Syn $ Free $ Or (mapView f a) (mapView f b) (fmap (getSyn . mapView f . Syn) next)
mapView f (Syn (Free (And a b next))) = Syn $ Free $ And (mapView f a) (mapView f b) (fmap (getSyn . mapView f . Syn) next)

forever :: Syn v a
forever = Syn $ liftF Forever

view :: v -> Syn v a
view v = Syn $ do
  liftF $ View v ()
  liftF Forever

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
  | S                    -- stop

unblock :: Monoid v => Syn v a -> IO (SynR v a)

unblock (Syn (Pure a))       = pure $ P a
unblock (Syn (Free Forever)) = pure S

unblock s@(Syn (Free (On (Event ref) next))) = readIORef ref >>= \case
  Just r  -> pure $ B $ unblock $ Syn $ next r
  Nothing -> pure $ B $ unblock s

unblock (Syn (Free (View v next))) = pure $ V v (unblock $ Syn next)

unblock (Syn (Free (Or a b next))) = go (unblock a) (unblock b) mempty mempty
  where
    go aSyn bSyn aPrV bPrV = do
      a' <- aSyn
      b' <- bSyn

      case (a', b') of
        -- one finished
        (P r, _) -> unblock $ Syn $ next r
        (_, P r) -> unblock $ Syn $ next r

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
        (S, S) -> pure S

        -- one stopped
        (S, B bNext) -> pure $ B $ go (pure S) bNext aPrV bPrV
        (B aNext, S) -> pure $ B $ go aNext (pure S) aPrV bPrV

unblock (Syn (Free (And a b next))) = go (unblock a) (unblock b) mempty mempty
  where
    go aSyn bSyn aPrV bPrV = do
      a' <- aSyn
      b' <- bSyn

      case (a', b') of
        -- both finished
        (P aR, P bR) -> unblock $ Syn $ next (aR <> bR)

        -- both views
        (V aV aNext, V bV bNext) -> pure $ V (aV <> bV) (go aNext bNext aV bV)

        -- left view, remaining variants
        (V aV aNext, B bNext) -> pure $ V (aV <> bPrV) (go aNext bNext aV bPrV)
        (V aV aNext, s)       -> pure $ V (aV <> bPrV) (go aNext (pure s) aV bPrV)

        -- right view, remaining variants
        (B aNext, V bV bNext)  -> pure $ V (aPrV <> bV) (go aNext bNext aPrV bV)
        (s, V bV bNext)        -> pure $ V (aPrV <> bV) (go (pure s) bNext aPrV bV)

        -- both blocked
        (B aNext, B bNext) -> pure $ B $ go aNext bNext aPrV bPrV

        -- one blocked, the other finished
        (B aNext, p@(P _)) -> pure $ B $ go aNext (pure p) aPrV bPrV
        (p@(P _), B bNext) -> pure $ B $ go (pure p) bNext aPrV bPrV

        -- stopped
        (S, _) -> pure S
        (_, S) -> pure S

--------------------------------------------------------------------------------

run :: Monoid v => Syn v () -> (v -> IO ()) -> IO (Event a -> a -> IO ())
run syn showView = do
  synR <- unblock syn

  mvSyn <- newMVar =<< case synR of
    V v synRIO -> showView v >> pure synRIO
    B synRIO   -> pure synRIO
    S          -> error "stopped"
    P ()       -> error "finished"

  pure $ \(Event ref) a -> do
    v <- modifyMVar mvSyn $ \synRIO -> do
      writeIORef ref (Just a)

      synR' <- synRIO

      synRIO' <- case synR' of
        V v synRIO' -> pure (synRIO', Just v)
        B synRIO'   -> pure (synRIO', Nothing)
        S           -> error "stopped"
        P ()        -> error "finished"

      writeIORef ref Nothing
      pure synRIO'

    for_ v showView
