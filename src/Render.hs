{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Render where

import Control.Applicative
import Control.Concurrent.MVar

import Data.Foldable
import Data.ObjectName
import Data.StateVar

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable

import qualified Graphics.Rendering.OpenGL as GL

import Syn

import GHC.Int
import GHC.Word

import Debug.Trace

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
--   (fbo, tex) <- unsafeNonBlockingIO $ do
--     fbo <- with1 $ glCreateFramebuffers 1
--     tex <- with1 $ glCreateTextures GL_TEXTURE_2D 1
-- 
--     glBindTexture GL_TEXTURE_2D tex
--     glTexStorage2D GL_TEXTURE_2D 1 GL_RGBA8 (width opts) (height opts)
--     glClearTexImage tex 0 GL_RGBA GL_UNSIGNED_BYTE undefined
-- 
--     pure (fbo, tex)

  let fbo = undefined
      tex = undefined
  mapView (combine fbo tex) $ mapView ((,Nothing) . Just) (a opts) <|> mapView ((Nothing,) . Just) (b opts)

  where
    combine fbo tex (Just aOut, Just bOut) = Out
      { tex = tex
      , render = \frame -> do
          render aOut frame
          render bOut frame
          -- glBindFramebuffer GL_FRAMEBUFFER fbo
      , destroy = undefined
      }

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

fsz :: Int
fsz = sizeOf (undefined :: GL.GLfloat)

rectBuffer :: IO GL.BufferObject
rectBuffer = do
  buf <- genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just buf
  withArrayLen verts $ \len ptr -> GL.bufferData GL.ArrayBuffer $= (fi (len * fsz), ptr, GL.StaticDraw)

  pure buf

  where
    verts :: [GL.GLfloat]
    verts =
        -- pos       -- uv
      [ -1, -1, 0,   0, 0
      ,  1, -1, 0,   1, 0
      , -1,  1, 0,   0, 1

      , -1,  1, 0,   0, 1
      ,  1, -1, 0,   1, 0
      ,  1,  1, 0,   1, 1
      ]

getDrawRect :: GL.BufferObject -> GL.AttribLocation -> Maybe (GL.AttribLocation) -> IO (IO (), IO ())
getDrawRect buf vLoc uvLoc = do
  vao <- genObjectName
  GL.bindVertexArrayObject $= Just vao

  GL.bindBuffer GL.ArrayBuffer $= Just buf

  GL.vertexAttribArray vLoc $= GL.Enabled
  GL.vertexAttribPointer vLoc $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (fi (5 * fsz)) nullPtr)

  for_ uvLoc $ \uvLoc' -> do
    GL.vertexAttribArray uvLoc' $= GL.Enabled
    GL.vertexAttribPointer uvLoc' $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float (fi (5 * fsz)) (nullPtr `plusPtr` (3 * fsz)))

  pure (render vao, deleteObjectName vao)

  where
    render vao = do
      GL.bindVertexArrayObject $= Just vao
      GL.drawArrays GL.Triangles 0 6

testrender2 :: IO (IO ())
testrender2 = do
  GL.debugMessageCallback $= Just dbg

  vertShader <- GL.createShader GL.VertexShader
  GL.shaderSourceBS vertShader $= vertShaderText
  GL.compileShader vertShader

  fragShader <- GL.createShader GL.FragmentShader
  GL.shaderSourceBS fragShader $= fragShaderText
  GL.compileShader fragShader

  program <- GL.createProgram
  GL.attachShader program vertShader
  GL.attachShader program fragShader
  GL.linkProgram program

  GL.programInfoLog program >>= print

  vPos <- get $ GL.attribLocation program "vPos"

  rectBuf <- rectBuffer
  (drawRect, _) <- getDrawRect rectBuf vPos Nothing

  pure $ do
    GL.viewport $= (GL.Position 0 0, GL.Size 640 480)
    GL.clearColor $= GL.Color4 0 0 0 0

    GL.currentProgram $= Just program
    drawRect
  where
    dbg msg@(GL.DebugMessage _ _ _ severity _) = do
      case severity of
        GL.DebugSeverityNotification -> pure ()
        _ -> traceIO $ show msg

    vertShaderText = mconcat
      [ "#version 330\n"
      , "in vec3 vPos;\n"
      , "void main()\n"
      , "{\n"
      , "    gl_Position = vec4(vPos, 1.0);\n"
      , "}\n"
      ]

    fragShaderText = mconcat
      [ "#version 330\n"
      , "out vec4 fragment;\n"
      , "void main()\n"
      , "{\n"
      , "    fragment = vec4(1.0, 0.0, 0.0, 1.0);\n"
      , "}\n"
      ]

testrender :: IO (IO ())
testrender = do
  vertBuf <- genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vertBuf
  withArrayLen verts $ \len ptr -> GL.bufferData GL.ArrayBuffer $= (fromIntegral (len * fsz), ptr, GL.StaticDraw)

  vertShader <- GL.createShader GL.VertexShader
  GL.shaderSourceBS vertShader $= vertShaderText
  GL.compileShader vertShader

  fragShader <- GL.createShader GL.FragmentShader
  GL.shaderSourceBS fragShader $= fragShaderText
  GL.compileShader fragShader

  program <- GL.createProgram
  GL.attachShader program vertShader
  GL.attachShader program fragShader
  GL.linkProgram program

  vPos <- get $ GL.attribLocation program "vPos"
  vCol <- get $ GL.attribLocation program "vCol"

  vertArray <- genObjectName
  GL.bindVertexArrayObject $= Just vertArray

  GL.vertexAttribArray vPos $= GL.Enabled
  GL.vertexAttribPointer vPos $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float (fi (5 * fsz)) nullPtr)

  GL.vertexAttribArray vCol $= GL.Enabled
  GL.vertexAttribPointer vCol $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (fi (5 * fsz)) (nullPtr `plusPtr` (2 * fsz)))

  -- vbuf <- with1 $ glGenBuffers 1
  -- glBindBuffer GL_ARRAY_BUFFER vbuf
  -- glBufferData GL_ARRAY_BUFFER _ _ GL_STATIC_DRAW

  pure $ do
    GL.viewport $= (GL.Position 0 0, GL.Size 640 480)
    GL.clearColor $= GL.Color4 0 0 0 0

    GL.currentProgram $= Just program
    GL.bindVertexArrayObject $= Just vertArray

    GL.drawArrays GL.Triangles 0 3
  where
    fi = fromIntegral
    fsz = sizeOf (undefined :: Float)

    verts :: [CFloat]
    verts = [ (-0.6), (-0.4), 1, 0, 0, 0.6, (-0.4), 0, 1, 0, 0, 0.6, 0, 0, 1 ]

    vertShaderText = mconcat
      [ "#version 330\n"
      , "in vec3 vCol;\n"
      , "in vec2 vPos;\n"
      , "out vec3 color;\n"
      , "void main()\n"
      , "{\n"
      , "    gl_Position = vec4(vPos, 0.0, 1.0);\n"
      , "    color = vCol;\n"
      , "}\n"
      ]

    fragShaderText = mconcat
      [ "#version 330\n"
      , "in vec3 color;\n"
      , "out vec4 fragment;\n"
      , "void main()\n"
      , "{\n"
      , "    fragment = vec4(color, 1.0);\n"
      , "}\n"
      ]
