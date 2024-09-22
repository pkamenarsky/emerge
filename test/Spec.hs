{-# LANGUAGE RankNTypes #-}

import Syn.Run
import qualified Syn.Threads as SynT

import Control.Applicative
import Control.Concurrent

import Data.IORef
import Data.Foldable (asum)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup

s1 :: Monad f => Alternative f => Semigroup (f String) => (forall a. String -> f a) -> (forall a. IO () -> f a -> f a) -> (t -> f ()) -> t -> f ()
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

test :: IO ()
test = do
  e <- newEvent :: IO (Event ())
  Just fire <- run (s1 view finalize on e) putStrLn

  fire e ()
  fire e ()
  fire e ()
  fire e ()
  fire e ()
  fire e ()

test2 :: IO ()
test2 = do
  e <- SynT.newEvent :: IO (SynT.Event ())
  (fire, go) <- SynT.run (s1 SynT.view (\_ x -> x) SynT.on e) putStrLn

  forkIO (go >> pure ())

  fire e ()
  fire e ()
  fire e ()
  fire e ()
  fire e ()
  fire e ()

main :: IO ()
main = do
  ref <- newIORef ""
  e <- newEvent :: IO (Event ())
  Just fire <- run (s1 e) (writeIORef ref)

  fire e ()
  cmp ref "AAC"
  fire e ()
  cmp ref "RRS"
  fire e ()
  cmp ref "BB"
  fire e ()
  cmp ref "C"

  where
    cmp ref should = do
      is <- readIORef ref
      if is == should
        then putStrLn "OK"
        else putStrLn $ "FAIL (" <> is <> " should be " <> should <> ")"

    s1 e = do
      pure ()
      r <- sconcat $ NE.fromList [ asum [ view "A", view "A", view "C", on e >> pure "R" ], on e >> pure "R", on e >> pure "S" ]
      _ <- asum [ view r, on e]
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
