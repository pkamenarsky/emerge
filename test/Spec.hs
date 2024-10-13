{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

import Syn
import Syn.Run
import qualified Syn.Threads as SynT

import Control.Applicative
import Control.Concurrent

import Data.IORef
import Data.Foldable (asum)
import qualified Data.List.NonEmpty as NE
import qualified Data.Monoid as M
import Data.Semigroup

s1 :: Monad f => Alternative f => Semigroup (f String) => (forall a. String -> f a) -> (forall a. IO () -> f a -> f a) -> (t -> f ()) -> t -> f a
s1 view finalize on e = do
  pure ()
  on e
  _ <- asum [ view "START", on e ]
  r <- sconcat $ NE.fromList
    [ asum
        [ finalize (putStrLn "FIN") $ view "A"
        , asum
            [ finalize (putStrLn "FIN2") $ view "A"
            , on e >> pure "T"
            ]
        , view "C"
        , on e >> pure "R"
        , finalize (putStrLn "FIN3") $ view "Z"
        ]
    , on e >> pure "R"
    , on e >> pure "S"
    ]
  _ <- asum [ view r, on e ]
  pure ()
  pure ()
  pure ()
  asum
    [ do
        pure ()
        pure ()
        _ <- view "B"
        pure ()
    , do
        pure ()
        view "B"
    , do
        pure ()
        _ <- on e
        pure ()
    ]
  pure ()
  pure ()
  pure ()
  asum [ view "C", on e ]
  s1 view finalize on e

_test :: IO ()
_test = do
  e <- newEvent :: IO (Event ())
  Just fire <- animate (fromArr $ toArr $ reinterpret $ s1 view finalize on e) putStrLn

  fire e ()
  fire e ()
  fire e ()
  fire e ()
  fire e ()
  fire e ()

  fire e ()

_test2 :: IO ()
_test2 = do
  e <- SynT.newEvent :: IO (SynT.Event ())
  (fire, go) <- SynT.run (s1 SynT.view (\_ x -> x) SynT.on e) putStrLn

  forkIO (go >> pure ())

  fire e ()
  fire e ()
  fire e ()
  fire e ()
  fire e ()

--------------------------------------------------------------------------------

cmp :: Eq a => Show a => IORef a -> a -> IO ()
cmp ref should = do
  is <- readIORef ref
  if is == should
    then putStrLn "OK"
    else putStrLn $ "FAIL (" <> show is <> " should be " <> show should <> ")"

testMapView :: IO ()
testMapView = do
  ref <- newIORef []
  e <- newEvent :: IO (Event ())
  Just fire <- animate (fromArr $ toArr $ reinterpret $ syn e) (writeIORef ref)

  fire e ()
  cmp ref [3]
  where
    syn e = do
      mapView (\as -> [sum as]) $ asum [ view [1], view [2], view' [] >> on e ]
      syn e

testAltSem :: IO ()
testAltSem = do
  ref <- newIORef ""
  e <- newEvent :: IO (Event ())
  Just fire <- animate (s1 e) (writeIORef ref)

  cmp ref "AAC"
  fire e ()
  cmp ref "RRS"
  fire e ()
  cmp ref "XZ"
  fire e ()
  cmp ref "YZ"
  fire e ()
  cmp ref "BB"
  fire e ()
  cmp ref "C"
  fire e ()
  cmp ref "AAC"

  where
    s1 e = do
      pure ()
      r <- sconcat $ NE.fromList [ asum [ view "A", view "A", view "C", view' "" >> on e >> pure "R" ], view' "" >> on e >> pure "R", view' "" >> on e >> pure "S" ]
      _ <- asum [ view r, view' "" >> on e]
      _ <- asum
        [ do
            _ <- asum [ pure () >> view "X", view' "" >> on e ]
            pure ()
            pure ()
            _ <- asum [ view "Y", view' "" >> on e ]
            pure ()
        , view "Z"
        ]
      pure ()
      pure ()
      pure ()
      asum
        [ do
            pure ()
            pure ()
            _ <- view "B"
            pure ()
        , do
            pure ()
            view "B"
        , do
            pure ()
            _ <- view' "" >> on e
            pure ()
        ]
      pure ()
      pure ()
      pure ()
      asum [ view "C", view' "" >> on e ]
      s1 e

main :: IO ()
main = do
  testAltSem
  testMapView
