{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Render where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Foldable
import Data.IORef
import Data.ObjectName
import Data.Machine.MealyT
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

import Syn
import Syn.Run

import GHC.Generics
import GHC.Int
import GHC.Word

import System.FilePath ((</>))
import qualified System.FilePath as Path

import Types

import Debug.Trace

--------------------------------------------------------------------------------

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

fsz :: Int
fsz = sizeOf (undefined :: GL.GLfloat)

-- Ops -------------------------------------------------------------------------

data OpOptions = OpOptions
  { opWidth :: Int32
  , opHeight :: Int32
  , opFormat :: GL.PixelInternalFormat
  , opClamp :: GL.Clamping
  }

defaultOpOptions :: OpOptions
defaultOpOptions = OpOptions
  { opWidth = 1024
  , opHeight = 1024
  , opFormat = GL.RGBA8
  , opClamp = GL.ClampToEdge
  }

data Op params = Op
  { opTex :: GL.TextureObject
  , opRender :: params -> IO ()
  , opDestroy :: IO ()
  }

data Out = Out
  { outTex :: GL.TextureObject
  , outRender :: IO ()
  }

--------------------------------------------------------------------------------

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

createFramebuffer :: OpOptions -> IO (GL.TextureObject, IO (), IO ())
createFramebuffer opts = do
  fbo <- genObjectName
  tex <- genObjectName

  GL.textureBinding GL.Texture2D $= Just tex

  GL.texImage2D GL.Texture2D GL.NoProxy 0 (opFormat opts) (GL.TextureSize2D (opWidth opts) (opHeight opts)) 0 (GL.PixelData GL.RGBA GL.UnsignedByte nullPtr)
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, opClamp opts)

  GL.bindFramebuffer GL.Framebuffer $= fbo
  GL.framebufferTexture2D GL.Framebuffer (GL.ColorAttachment 0) GL.Texture2D tex 0

  pure (tex, bind fbo, destroy fbo tex)

  where
    bind fbo = do
      GL.bindFramebuffer GL.Framebuffer $= fbo
      GL.viewport $= (GL.Position 0 0, GL.Size (opWidth opts) (opHeight opts))

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

--------------------------------------------------------------------------------

data BlitParams = BlitParams
  { blitSource :: Tex1
  } deriving Generic

instance ShaderParam BlitParams where

blit :: RectBuffer -> GL.Size -> IO (GL.TextureObject -> IO (), IO ())
blit rectBuf viewport = do
  (attribs, bindShader, destroyShader) <- createShader vertT fragT True
  (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

  pure
    ( \tex -> do
        GL.viewport $= (GL.Position 0 0, viewport)
        GL.clearColor $= GL.Color4 0 0 0 0

        GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject

        bindShader $ BlitParams (Tex1 tex)
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
      , "uniform sampler2D blitSource;\n"
      , "in vec2 uv;"
      , "out vec4 fragment;"
      , "void main()"
      , "{"
      , "  fragment = texture2D(blitSource, uv);"
      , "}"
      ]

-- Ops (fill) ------------------------------------------------------------------

data FillParams = FillParams
  { foColor :: GL.Color4 Float
  } deriving Generic

instance ShaderParam FillParams where

fill :: RectBuffer -> OpOptions -> IO (Op FillParams)
fill rectBuf opts = do
  (tex, bindFBO, destroyFBO) <- createFramebuffer opts
  (attribs, bindShader, destroyShader) <- createShader vertT fragT False

  (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

  pure $ Op
    { opTex = tex
    , opRender = \params -> do
        bindFBO
        bindShader params
        drawRect
    , opDestroy = do
        destroyFBO
        destroyShader
        destroyDrawRect
    }
  where
    vertT = T.unlines
      [ "#version 460"
      , "in vec3 a_pos;"
      , "void main() {"
      , "    gl_Position = vec4(a_pos, 1.0);"
      , "}"
      ]

    fragT = T.unlines
      [ "#version 460"
      , "#include \"assets/lygia/generative/cnoise.glsl\""
      , "out vec4 fragment;"
      , "uniform vec4 foColor;\n"
      , "void main() {"
      , "  fragment = foColor;"
      , "}"
      ]

fillSyn :: MonadIO m => RectBuffer -> OpOptions -> Signal FillParams -> Syn [Out] m a
fillSyn rectBuf opts params = do
  Op tex render destroy <- unsafeNonBlockingIO $ fill rectBuf opts

  finalize (liftIO destroy) $ view $ pure $ Out
    { outTex = tex
    , outRender = signalValue params >>= render
    }

-- Ops (circle) ----------------------------------------------------------------

data CircleParams = CircleParams
  { cpColor :: GL.Color4 Float
  , cpCenter :: GL.Vertex2 Float
  , cpRadius :: Float
  } deriving Generic

instance ShaderParam CircleParams where

circle :: RectBuffer -> OpOptions -> IO (Op CircleParams)
circle rectBuf opts = do
  (tex, bindFBO, destroyFBO) <- createFramebuffer opts
  (attribs, bindShader, destroyShader) <- createShader vertT fragT True

  (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

  pure $ Op
    { opTex = tex
    , opRender = \params -> do
        bindFBO
        bindShader params
        drawRect
    , opDestroy = do
        destroyFBO
        destroyShader
        destroyDrawRect
    }
  where
    vertT = T.unlines
      [ "#version 460"
      , "in vec3 a_pos;"
      , "in vec2 a_uv;"
      , "out vec2 uv;"
      , "void main() {"
      , "  gl_Position = vec4(a_pos, 1.0);"
      , "  uv = a_uv;"
      , "}"
      ]

    fragT = T.unlines
      [ "#version 460"
      , "#include \"assets/lygia/generative/cnoise.glsl\""
      , "out vec4 fragment;"
      , "in vec2 uv;"
      , "uniform vec4 cpColor;\n"
      , "uniform vec2 cpCenter;\n"
      , "uniform float cpRadius;\n"
      , "void main() {"
      , "  float dist = distance(uv, cpCenter);"
      , "  float delta = fwidth(dist);"
      , "  float alpha = smoothstep(cpRadius - delta, cpRadius, dist);"
      , "  fragment = cpColor * (1 - alpha);"
      , "}"
      ]

circleSyn :: MonadIO m => RectBuffer -> OpOptions -> Signal CircleParams -> Syn [Out] m a
circleSyn rectBuf opts params = do
  Op tex render destroy <- unsafeNonBlockingIO $ circle rectBuf opts

  finalize (liftIO destroy) $ view $ pure $ Out
    { outTex = tex
    , outRender = signalValue params >>= render
    }

-- Ops (blend) -----------------------------------------------------------------

data BlendMode = Add | Mul

data BlendOptions = BlendOptions
  { bpOpOptions :: OpOptions
  , bpMode :: BlendMode
  } deriving Generic

defaultBlendOptions :: BlendOptions
defaultBlendOptions = BlendOptions
  { bpOpOptions = defaultOpOptions
  , bpMode = Add
  }

data BlendParams = BlendParams
  { bpFactor :: Float
  , bpTex1 :: Tex1
  , bpTex2 :: Tex2
  } deriving Generic

instance ShaderParam BlendParams where

blend :: RectBuffer -> BlendOptions -> IO (Op BlendParams)
blend rectBuf opts = do
  (tex, bindFBO, destroyFBO) <- createFramebuffer (bpOpOptions opts)
  (attribs, bindShader, destroyShader) <- createShader vertT fragT True

  (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

  pure $ Op
    { opTex = tex
    , opRender = \params -> do
        bindFBO
        bindShader params
        drawRect
    , opDestroy = do
        destroyFBO
        destroyShader
        destroyDrawRect
    }
  where
    vertT = T.unlines
      [ "#version 460"
      , "in vec3 a_pos;"
      , "in vec2 a_uv;"
      , "out vec2 uv;"
      , "void main() {"
      , "    gl_Position = vec4(a_pos, 1.0);"
      , "    uv = a_uv;"
      , "}"
      ]

    fragT = T.unlines
      [ "#version 460"
      , "#include \"assets/lygia/generative/cnoise.glsl\""
      , "in vec2 uv;"
      , "out vec4 fragment;"
      , "uniform sampler2D bpTex1;"
      , "uniform sampler2D bpTex2;"
      , "uniform float bpFactor;\n"
      , "void main() {"
      , case bpMode opts of
          Add -> "  fragment = texture2D(bpTex1, uv) * bpFactor + texture2D(bpTex2, uv) * (1 - bpFactor);"
          Mul -> "  fragment = texture2D(bpTex1, uv) * texture2D(bpTex2, uv);"
      , "}"
      ]

data BlendParamsSyn = BlendParamsSyn
  { bpsFactor :: Float
  }

blendSyn :: MonadIO m => RectBuffer -> BlendOptions -> Signal BlendParamsSyn -> Syn [Out] m a -> Syn [Out] m a -> Syn [Out] m a
blendSyn rectBuf opts params a b = do
  Op tex render destroy <- unsafeNonBlockingIO $ blend rectBuf opts

  let out [aOut, bOut] = pure $ Out
        { outTex = tex
        , outRender = do
            outRender aOut
            outRender bOut
            params' <- signalValue params
            render $ BlendParams
              { bpFactor = bpsFactor params'
              , bpTex1 = Tex1 $ outTex aOut
              , bpTex2 = Tex2 $ outTex bOut
              }
        }
      out l = pure $ Out
        { outTex = tex
        , outRender = print (length l) >> pure ()
        }

  finalize (liftIO destroy) $ mapView out $ asum [ a, b ]

--------------------------------------------------------------------------------

data TestOpts = TestOpts
  { u_tColor :: GL.Color3 Float
  , u_tTime :: Float
  } deriving Generic

instance ShaderParam TestOpts where
  
testrender :: IO (IO (), IO ())
testrender = do
  GL.debugMessageCallback $= Just dbg
  rectBuf <- createRectBuffer

  ---

  (tex, bindFBO, destroyFBO) <- createFramebuffer (OpOptions 512 512 GL.RGBA8 GL.ClampToEdge)
  (attribs, bindShader, destroyShader) <- createShader vertT fragT True

  (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

  (blitToScreen, destroyBlit) <- blit rectBuf (GL.Size 640 480)

  ref <- newIORef 0

  pure
    ( do
        t <- atomicModifyIORef ref $ \t' -> (t' + 0.01, t')

        GL.viewport $= (GL.Position 0 0, GL.Size 640 480)
        GL.clearColor $= GL.Color4 0 0 0 0

        bindFBO
        bindShader $ TestOpts (GL.Color3 1 1 1) t
        drawRect

        blitToScreen tex
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
      , "uniform vec3 u_tColor;\n"
      , "uniform float u_tTime;\n"
      , "in vec2 uv;"
      , "void main()"
      , "{"
      , "  float u_period = 10.;"
      , "  float u_time = 123.;"
      , "  float u_scale = 0.2;"
      , "  float u_offset = 0.1;"
      , "  float c = cnoise(vec3(uv * u_period, u_time + u_tTime)) * u_scale + u_offset;"
      , "  fragment = vec4(u_tColor * c, 1.);"
      , "}"
      ]

--------------------------------------------------------------------------------

-- TODO: don't include same file twice
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
