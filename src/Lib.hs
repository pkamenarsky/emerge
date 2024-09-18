{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lib where

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
  = View v next
  | forall a. On (Event a) (a -> next)
  | forall a. Or (Syn v a) (Syn v a) (a -> next)
  | forall a. Monoid a => And (Syn v a) (Syn v a) (a -> next)
  | Forever

deriving instance Functor (SynF v)

newtype Syn v a = Syn (Free (SynF v) a)
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

unblock (Syn (Pure a))       = pure $ P a
unblock (Syn (Free Forever)) = pure S

unblock s@(Syn (Free (On (Event ref) next))) = readIORef ref >>= \case
  Just r  -> unblock $ Syn $ next r
  Nothing -> pure $ B $ unblock s

unblock (Syn (Free (View v next))) = pure $ V v (unblock $ Syn next)

unblock (Syn (Free (Or a b next))) = go (unblock a) (unblock b) mempty mempty
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
        (P aR, P bR) -> pure $ V mempty (unblock $ Syn $ next (aR <> bR))

        -- both views
        (V aV aNext, V bV bNext) -> pure $ V (aV <> bV) (go aNext bNext aV bV)

        -- left view, remaining variants
        (V aV aNext, B bNext) -> pure $ V (aV <> bPrV) (go aNext bNext aV bPrV)
        (V aV aNext, p@(P _)) -> pure $ V (aV <> bPrV) (go aNext (pure p) aV bPrV)
        (V aV aNext, S)       -> pure $ V (aV <> bPrV) (go aNext (pure S) aV bPrV)

        -- right view, remaining variants
        (B aNext, V bV bNext)  -> pure $ V (aPrV <> bV) (go aNext bNext aPrV bV)
        (p@(P _), V bV bNext)  -> pure $ V (aPrV <> bV) (go (pure p) bNext aPrV bV)
        (S, V bV bNext)        -> pure $ V (aPrV <> bV) (go (pure S) bNext aPrV bV)

        -- both blocked
        (B aNext, B bNext) -> pure $ B $ go aNext bNext aPrV bPrV

        -- one blocked, the other finished
        (B aNext, p@(P _)) -> pure $ B $ go aNext (pure p) aPrV bPrV
        (p@(P _), B bNext) -> pure $ B $ go (pure p) bNext aPrV bPrV

        -- both stopped
        (S, S) -> pure S

        -- one stopped, the other finished
        (S, P _) -> pure S
        (P _, S) -> pure S

        -- one stopped, the other blocked
        (S, B bNext) -> pure $ B $ go (pure S) bNext aPrV bPrV
        (B aNext, S) -> pure $ B $ go aNext (pure S) aPrV bPrV

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
