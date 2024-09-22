{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Render where

import Control.Applicative
import Control.Concurrent.MVar

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Foldable
import Data.ObjectName
import Data.StateVar
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Text (Text)

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable

import qualified Graphics.Rendering.OpenGL as GL

import Syn.Run

import GHC.Int
import GHC.Word

import System.FilePath ((</>))
import qualified System.FilePath as Path

import Debug.Trace

newtype Frame = Frame Int
  deriving (Eq, Ord)

data Out = Out
  { outTex :: Word32
  , outRender :: Frame -> IO ()
  , outDestroy :: IO ()
  }

data CompositeOpts = CompositeOpts
  { coptsMode :: Int
  , coptsWidth :: Int32
  , coptsHeight :: Int32
  , coptsA :: Syn Out IO ()
  , coptsB :: Syn Out IO ()
  }

with1 :: Storable a => (Ptr a -> IO ()) -> IO a
with1 f = alloca $ \r -> f r >> peek r

composite :: CompositeOpts -> Syn Out IO ()
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
  mapView (combine fbo tex) $ mapView ((,Nothing) . Just) (coptsA opts) <|> mapView ((Nothing,) . Just) (coptsA opts)

  where
    combine fbo tex (Just aOut, Just bOut) = Out
      { outTex = tex
      , outRender = \frame -> do
          outRender aOut frame
          outRender bOut frame
          -- glBindFramebuffer GL_FRAMEBUFFER fbo
      , outDestroy = undefined
      }

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

fsz :: Int
fsz = sizeOf (undefined :: GL.GLfloat)

createRectBuffer :: IO GL.BufferObject
createRectBuffer = do
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

createDrawRect :: GL.BufferObject -> GL.AttribLocation -> Maybe (GL.AttribLocation) -> IO (IO (), IO ())
createDrawRect buf vLoc uvLoc = do
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

createFramebuffer :: Int32 -> Int32 -> GL.PixelInternalFormat -> GL.Clamping -> IO (IO (), IO ())
createFramebuffer width height ifmt clamp = do
  fbo <- genObjectName
  tex <- genObjectName

  GL.textureBinding GL.Texture2D $= Just tex

  GL.texImage2D GL.Texture2D GL.NoProxy 0 ifmt (GL.TextureSize2D width height) 0 (GL.PixelData GL.RGBA GL.UnsignedByte nullPtr)
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, clamp)

  GL.bindFramebuffer GL.Framebuffer $= fbo
  GL.framebufferTexture2D GL.Framebuffer (GL.ColorAttachment 0) GL.Texture2D tex 0

  pure (bind fbo, destroy fbo tex)

  where
    bind fbo = do
      GL.bindFramebuffer GL.Framebuffer $= fbo
      GL.viewport $= (GL.Position 0 0, GL.Size width height)

    destroy fbo tex = do
      deleteObjectName fbo
      deleteObjectName tex

testrender2 :: IO (IO ())
testrender2 = do
  GL.debugMessageCallback $= Just dbg

  vertShader <- GL.createShader GL.VertexShader
  GL.shaderSourceBS vertShader $= vertShaderText
  GL.compileShader vertShader

  fragShader <- GL.createShader GL.FragmentShader
  fragShaderText' <- resolveGLSL fragShaderText
  GL.shaderSourceBS fragShader $= T.encodeUtf8 fragShaderText'
  GL.compileShader fragShader

  program <- GL.createProgram
  GL.attachShader program vertShader
  GL.attachShader program fragShader
  GL.linkProgram program

  GL.programInfoLog program >>= print

  vPos <- get $ GL.attribLocation program "vPos"
  vUV <- get $ GL.attribLocation program "vUV"

  rectBuf <- createRectBuffer
  (drawRect, _) <- createDrawRect rectBuf vPos (Just vUV)

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
      [ "#version 460\n"
      , "in vec3 vPos;\n"
      , "in vec2 vUV;\n"
      , "out vec2 uv;\n"
      , "void main()\n"
      , "{\n"
      , "    gl_Position = vec4(vPos, 1.0);\n"
      , "    uv = vUV;\n"
      , "}\n"
      ]

    fragShaderText = T.unlines
      [ "#version 460"
      , "#include \"assets/lygia/generative/cnoise.glsl\""
      , "out vec4 fragment;"
      , "in vec2 uv;"
      , "void main()"
      , "{"
      , "  float u_period = 10.;"
      , "  float u_time = 123.;"
      , "  float u_scale = 0.2;"
      , "  float u_offset = 0.1;"
      , "  float c = cnoise(vec3(uv * u_period, u_time)) * u_scale + u_offset;"
      , "  fragment = vec4(c, c, c, 1.);"
      , "}"
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

--------------------------------------------------------------------------------

-- TODO: embed basePath directory in binary using TH
resolveGLSLFile :: FilePath -> IO Text
resolveGLSLFile path = do
  file <- T.readFile path

  T.unlines <$> sequence
    [ case T.stripPrefix "#include \"" line >>= T.stripSuffix "\"" of
        Just include -> resolveGLSLFile (basePath </> T.unpack include)
        Nothing      -> pure line
    | line <- T.lines file
    ]

  where
    basePath = Path.takeDirectory path

resolveGLSL :: T.Text -> IO Text
resolveGLSL glsl = do
  T.unlines <$> sequence
    [ case T.stripPrefix "#include \"" line >>= T.stripSuffix "\"" of
        Just include -> resolveGLSLFile (T.unpack include)
        Nothing      -> pure line
    | line <- T.lines glsl
    ]
