{-# LANGUAGE TupleSections #-}

module Render where

import Control.Applicative
import Control.Concurrent.MVar

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Graphics.GL.Core45

import Syn

import GHC.Int

newtype Frame = Frame Int
  deriving (Eq, Ord)

data Out = Out
  { render :: Frame -> IO ()
  , destroy :: IO ()
  }

data CompositeOpts = CompositeOpts
  { mode :: Int
  , width :: Int32
  , height :: Int32
  , a :: Syn Out ()
  , b :: Syn Out ()
  }

with1 :: Storable a => (Ptr a -> IO ()) -> IO a
with1 f = alloca $ \r -> f r >> peek r

composite :: CompositeOpts -> Syn Out ()
composite opts = do
  _ <- unsafeNonBlockingIO $ do
    fbo <- with1 $ glCreateFramebuffers 1
    tex <- with1 $ glCreateTextures GL_TEXTURE_2D 1

    glBindTexture GL_TEXTURE_2D tex
    glTexStorage2D GL_TEXTURE_2D 1 GL_RGBA8 (width opts) (height opts)
    glClearTexImage tex 0 GL_RGBA GL_UNSIGNED_BYTE undefined

    pure ()

  mapView combine $ mapView ((,Nothing) . Just) (a opts) <|> mapView ((Nothing,) . Just) (b opts)

  where
    combine (Nothing, Nothing) = undefined
