{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Render where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Foldable
import Data.IORef
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

import GHC.Generics
import GHC.Int
import GHC.Word

import System.FilePath ((</>))
import qualified System.FilePath as Path

import Types

import Debug.Trace

data Out = Out
  { outTex :: Word32
  , outRender :: IO ()
  }

data CompositeOpts = CompositeOpts
  { coptsMode :: Int
  , coptsWidth :: Int32
  , coptsHeight :: Int32
  , coptsA :: Syn Out IO ()
  , coptsB :: Syn Out IO ()
  }

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
      , outRender = do
          outRender aOut
          outRender bOut
      }

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

fsz :: Int
fsz = sizeOf (undefined :: GL.GLfloat)

data RectBuffer = RectBuffer GL.BufferObject

createRectBuffer :: IO RectBuffer
createRectBuffer = RectBuffer <$> do
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

createDrawRect :: RectBuffer -> ShaderAttribs -> IO (IO (), IO ())
createDrawRect (RectBuffer buf) sattrs = do
  vao <- genObjectName
  GL.bindVertexArrayObject $= Just vao

  GL.bindBuffer GL.ArrayBuffer $= Just buf

  GL.vertexAttribArray (sa_pos sattrs) $= GL.Enabled
  GL.vertexAttribPointer (sa_pos sattrs) $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (fi (5 * fsz)) nullPtr)

  for_ (sa_uv sattrs) $ \sa_uv' -> do
    GL.vertexAttribArray sa_uv' $= GL.Enabled
    GL.vertexAttribPointer sa_uv' $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float (fi (5 * fsz)) (nullPtr `plusPtr` (3 * fsz)))

  pure (render vao, deleteObjectName vao)

  where
    render vao = do
      GL.bindVertexArrayObject $= Just vao
      GL.drawArrays GL.Triangles 0 6

createFramebuffer :: Int32 -> Int32 -> GL.PixelInternalFormat -> GL.Clamping -> IO (GL.TextureObject, IO (), IO ())
createFramebuffer width height ifmt clamp = do
  fbo <- genObjectName
  tex <- genObjectName

  GL.textureBinding GL.Texture2D $= Just tex

  GL.texImage2D GL.Texture2D GL.NoProxy 0 ifmt (GL.TextureSize2D width height) 0 (GL.PixelData GL.RGBA GL.UnsignedByte nullPtr)
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, clamp)

  GL.bindFramebuffer GL.Framebuffer $= fbo
  GL.framebufferTexture2D GL.Framebuffer (GL.ColorAttachment 0) GL.Texture2D tex 0

  pure (tex, bind fbo, destroy fbo tex)

  where
    bind fbo = do
      GL.bindFramebuffer GL.Framebuffer $= fbo
      GL.viewport $= (GL.Position 0 0, GL.Size width height)

    destroy fbo tex = do
      deleteObjectName fbo
      deleteObjectName tex

data ShaderAttribs = ShaderAttribs
  { sa_pos :: GL.AttribLocation
  , sa_uv :: Maybe GL.AttribLocation
  }

createShader :: ShaderParam p => Text -> Text -> Bool -> IO (ShaderAttribs, p -> IO (), IO ())
createShader vertT fragT uv = do
  vertShader <- GL.createShader GL.VertexShader
  vertT' <- resolveGLSL vertT
  GL.shaderSourceBS vertShader $= T.encodeUtf8 vertT'
  GL.compileShader vertShader

  fragShader <- GL.createShader GL.FragmentShader
  fragT' <- resolveGLSL fragT
  GL.shaderSourceBS fragShader $= T.encodeUtf8 fragT'
  GL.compileShader fragShader

  program <- GL.createProgram
  GL.attachShader program vertShader
  GL.attachShader program fragShader
  GL.linkProgram program

  ls <- GL.linkStatus program

  when (not ls) $ do
    ilog <- GL.programInfoLog program
    error $ "createShader: " <> ilog

  a_pos <- get $ GL.attribLocation program "a_pos"
  a_uv <- if uv then fmap Just $ get $ GL.attribLocation program "a_uv" else pure Nothing

  set <- shaderParam program

  return (ShaderAttribs { sa_pos = a_pos, sa_uv = a_uv }, bind program set, destroy vertShader fragShader program)
    where
      bind program setParams params = do
        GL.currentProgram $= Just program
        setParams params

      destroy vertShader fragShader program = do
        GL.deleteObjectName vertShader
        GL.deleteObjectName fragShader
        GL.deleteObjectName program

data Op1 p = Op1 GL.TextureObject p

instance ShaderParam p => ShaderParam (Op1 p) where
  shaderParam program = do
    loc <- GL.uniformLocation program "u_tex"
    set <- shaderParam program

    pure $ \(Op1 tex p) -> do
      GL.activeTexture $= GL.TextureUnit 0
      GL.textureBinding GL.Texture2D $= Just tex
      GL.uniform loc $= GL.TextureUnit 0

      set p

data Op2 p = Op2 GL.TextureObject GL.TextureObject p

instance ShaderParam p => ShaderParam (Op2 p) where
  shaderParam program = do
    loc1 <- GL.uniformLocation program "u_tex1"
    loc2 <- GL.uniformLocation program "u_tex2"
    set <- shaderParam program

    pure $ \(Op2 tex1 tex2 p) -> do
      GL.activeTexture $= GL.TextureUnit 0
      GL.textureBinding GL.Texture2D $= Just tex1
      GL.uniform loc1 $= GL.TextureUnit 0

      GL.activeTexture $= GL.TextureUnit 1
      GL.textureBinding GL.Texture2D $= Just tex2
      GL.uniform loc2 $= GL.TextureUnit 1

      set p

blit :: RectBuffer -> GL.TextureObject -> GL.Size -> IO (IO (), IO ())
blit rectBuf tex viewport = do
  (attribs, bindShader, destroyShader) <- createShader vertT fragT True
  (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

  pure
    ( do
        GL.viewport $= (GL.Position 0 0, viewport)
        GL.clearColor $= GL.Color4 0 0 0 0

        GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject

        bindShader $ Op1 tex ()
        drawRect

    , do
        destroyShader
        destroyDrawRect
    ) 
  where
    vertT = mconcat
      [ "#version 460\n"
      , "in vec3 a_pos;\n"
      , "in vec2 a_uv;\n"
      , "out vec2 uv;\n"
      , "void main()\n"
      , "{\n"
      , "    gl_Position = vec4(a_pos, 1.0);\n"
      , "    uv = a_uv;\n"
      , "}\n"
      ]

    fragT = T.unlines
      [ "#version 460"
      , "uniform sampler2D u_op1Tex;\n"
      , "in vec2 uv;"
      , "out vec4 fragment;"
      , "void main()"
      , "{"
      , "  fragment = texture2D(u_op1Tex, uv);"
      , "}"
      ]

data FillOpts = FillOpts
  { u_foColor :: GL.Color3 Float
  , u_foTime :: Float
  } deriving Generic

instance ShaderParam FillOpts where
  
testrender2 :: IO (IO (), IO ())
testrender2 = do
  GL.debugMessageCallback $= Just dbg
  rectBuf <- createRectBuffer

  ---

  (tex, bindFBO, destroyFBO) <- createFramebuffer 512 512 GL.RGBA8 GL.ClampToEdge
  (attribs, bindShader, destroyShader) <- createShader vertT fragT True

  (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

  (blitToScreen, destroyBlit) <- blit rectBuf tex (GL.Size 640 480)

  ref <- newIORef 0

  pure
    ( do
        t <- atomicModifyIORef ref $ \t' -> (t' + 0.01, t')

        GL.viewport $= (GL.Position 0 0, GL.Size 640 480)
        GL.clearColor $= GL.Color4 0 0 0 0

        bindFBO
        bindShader $ FillOpts (GL.Color3 1 1 1) t
        drawRect

        blitToScreen
    , do
        destroyFBO
        destroyShader
        destroyDrawRect
        destroyBlit
    )
  where
    dbg msg@(GL.DebugMessage _ _ _ severity _) = do
      case severity of
        GL.DebugSeverityNotification -> pure ()
        _ -> traceIO $ show msg

    vertT = mconcat
      [ "#version 460\n"
      , "in vec3 a_pos;\n"
      , "in vec2 a_uv;\n"
      , "out vec2 uv;\n"
      , "void main()\n"
      , "{\n"
      , "    gl_Position = vec4(a_pos, 1.0);\n"
      , "    uv = a_uv;\n"
      , "}\n"
      ]

    fragT = T.unlines
      [ "#version 460"
      , "#include \"assets/lygia/generative/cnoise.glsl\""
      , "out vec4 fragment;"
      , "uniform vec3 u_foColor;\n"
      , "uniform float u_foTime;\n"
      , "in vec2 uv;"
      , "void main()"
      , "{"
      , "  float u_period = 10.;"
      , "  float u_time = 123.;"
      , "  float u_scale = 0.2;"
      , "  float u_offset = 0.1;"
      , "  float c = cnoise(vec3(uv * u_period, u_time + u_foTime)) * u_scale + u_offset;"
      , "  fragment = vec4(u_foColor * c, 1.);"
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
