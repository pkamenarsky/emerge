{-# LANGUAGE TupleSections #-}

module Render where

import Control.Applicative

import Syn

newtype Frame = Frame Int
  deriving (Eq, Ord)

data Out = Out { render :: Frame -> IO () }

instance Semigroup Out where
  a <> b = Out $ \frame -> render a frame >> render b frame

instance Monoid Out where
  mempty = Out $ const $ pure ()

data CompositeOpts = CompositeOpts
  { mode :: Int
  , a :: Syn Out ()
  , b :: Syn Out ()
  }

composite :: CompositeOpts -> Syn Out ()
composite opts = mapView _ $ mapView ((,Nothing) . Just) (a opts) <|> mapView ((Nothing,) . Just) (b opts)
