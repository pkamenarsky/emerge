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
import GHC.Word

newtype Frame = Frame Int
  deriving (Eq, Ord)

data Out = Out
  { tex :: Word32
  , render :: Frame -> IO ()
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
  (fbo, tex) <- unsafeNonBlockingIO $ do
    fbo <- with1 $ glCreateFramebuffers 1
    tex <- with1 $ glCreateTextures GL_TEXTURE_2D 1

    glBindTexture GL_TEXTURE_2D tex
    glTexStorage2D GL_TEXTURE_2D 1 GL_RGBA8 (width opts) (height opts)
    glClearTexImage tex 0 GL_RGBA GL_UNSIGNED_BYTE undefined

    pure (fbo, tex)

  mapView (combine fbo tex) $ mapView ((,Nothing) . Just) (a opts) <|> mapView ((Nothing,) . Just) (b opts)

  where
    combine fbo tex (Just aOut, Just bOut) = Out
      { tex = tex
      , render = \frame -> do
          render aOut frame
          render bOut frame
          glBindFramebuffer GL_FRAMEBUFFER fbo
      , destroy = undefined
      }

--------------------------------------------------------------------------------

data NodeType
  = Composite CompositeOptsN

data Node = Node
  { t :: NodeType
  , children :: [Node]
  }

data CompositeOptsN = CompositeOptsN
  { modeN :: Int
  , widthN :: Int32
  , heightN :: Int32
  }

compositeN :: CompositeOptsN -> Syn Node a -> Syn Node a -> Syn Node a
compositeN opts a b = mapView node (mapView pure a <|> mapView pure b)
  where
    node :: [Node] -> Node
    node children = Node
      { t = Composite opts
      , children = children
      }
