import Syn

import Data.IORef

import Data.Foldable (asum)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup

main :: IO ()
main = do
  ref <- newIORef ""
  e <- newEvent :: IO (Event ())
  fire <- run (s1 e) (writeIORef ref)

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
