{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Common where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.State as ST

import Codec.Picture

import qualified Data.Array.IO as A
import qualified Data.Array.Storable as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Foldable
import Data.ObjectName
import Data.Maybe (fromMaybe)
import Data.StateVar
import qualified Data.Set as S
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Vector.Storable as V

import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

import Event
import Syn

import GHC.Int
import GHC.Word

import System.FilePath ((</>))
import qualified System.FilePath as Path

import Types

--------------------------------------------------------------------------------

tf :: Real a => Fractional b => a -> b
tf = realToFrac

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

fsz :: Int
fsz = sizeOf (undefined :: GL.GLfloat)

ranged :: Float -> Float -> Float -> Float -> Float -> Float
ranged a b c d x = a + (b - a) * ((x - c) / (d - c))

-- Ops -------------------------------------------------------------------------

data OpOptions = OpOptions
  { opWidth :: Int32
  , opHeight :: Int32
  , opFormat :: GL.PixelInternalFormat
  , opClamp :: GL.Clamping
  }

instance Default OpOptions where
  def = OpOptions
    { opWidth = 1024
    , opHeight = 1024
    , opFormat = GL.RGBA8
    , opClamp = GL.ClampToEdge
    }

resVec2 :: OpOptions -> Text
resVec2 opts = [i|vec2 (#{opWidth opts}, #{opHeight opts})|];

data ShaderAttribs = ShaderAttribs
  { saPos :: GL.AttribLocation
  , saUv :: Maybe GL.AttribLocation
  , saProgram :: GL.Program
  }

data Out = Out
  { outTex :: GL.TextureObject
  , outRender :: IO ()
  }

data OpContext = OpContext
  { ctxOptions :: OpOptions
  , ctxRectBuf :: GL.BufferObject
  , ctxEventCtx :: EventContext
  , ctxSignalCtx :: SignalContext
  }

newtype Op a = Op { unOp :: Syn [Out] (ReaderT OpContext IO) a }
  deriving (Functor, Applicative, Monad, Alternative, Semigroup)

on_ :: MonadIO m => EventFilter a -> Syn v (ReaderT OpContext m) a
on_ e = do
  ctx <- lift ask
  ctxOn (ctxEventCtx ctx) e

on :: EventFilter a -> Op a
on e = Op $ do
  ctx <- lift ask
  ctxOn (ctxEventCtx ctx) e

signals :: (SignalContext -> Op a) -> Op a
signals f = Op $ do
  ctx <- lift ask
  unOp $ f (ctxSignalCtx ctx)

--------------------------------------------------------------------------------

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

createDrawRect :: GL.BufferObject -> ShaderAttribs -> IO (IO (), IO ())
createDrawRect buf sattrs = do
  vao <- genObjectName

  GL.bindVertexArrayObject $= Just vao

  GL.bindBuffer GL.ArrayBuffer $= Just buf

  GL.vertexAttribArray (saPos sattrs) $= GL.Enabled
  GL.vertexAttribPointer (saPos sattrs) $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (fi (5 * fsz)) nullPtr)

  for_ (saUv sattrs) $ \saUv' -> do
    GL.vertexAttribArray saUv' $= GL.Enabled
    GL.vertexAttribPointer saUv' $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float (fi (5 * fsz)) (nullPtr `plusPtr` (3 * fsz)))

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

  GL.clearColor $= GL.Color4 0 0 0 0
  GL.clear [ GL.ColorBuffer ]

  pure (tex, bind fbo, destroy fbo tex)

  where
    bind fbo = do
      GL.bindFramebuffer GL.Framebuffer $= fbo
      GL.viewport $= (GL.Position 0 0, GL.Size (opWidth opts) (opHeight opts))

    destroy fbo tex = do
      deleteObjectName fbo
      deleteObjectName tex

allocArray :: Int -> Int -> IO (A.StorableArray Int Word8)
allocArray width height = A.newArray (0, width * height * 4) 0 :: IO (A.StorableArray Int Word8)

readImageFromFramebuffer :: GL.FramebufferObject -> A.StorableArray Int Word8 -> Int -> Int -> IO (Either String BL.ByteString)
readImageFromFramebuffer fbo pixels width height = do
  GL.bindFramebuffer GL.Framebuffer $= fbo
  GL.readBuffer $= GL.FBOColorAttachment 0

  image <- A.withStorableArray pixels $ \ptr -> do
    GL.readPixels (GL.Position 0 0) (GL.Size (fromIntegral width) (fromIntegral height)) (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
    fptr <- newForeignPtr_ ptr
    pure $ Image width height $ V.unsafeFromForeignPtr fptr 0 (width * height * 4)
  
  pure $ encodeDynamicPng $ ImageRGBA8 image

readImageFromTexture :: GL.TextureObject -> A.StorableArray Int Word8 -> Int -> Int -> IO (Either String BL.ByteString)
readImageFromTexture tex pixels width height = do
  GL.textureBinding GL.Texture2D $= Just tex

  image <- A.withStorableArray pixels $ \ptr -> do
    GL.getTexImage GL.Texture2D 0 (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
    fptr <- newForeignPtr_ ptr
    pure $ Image width height $ V.unsafeFromForeignPtr fptr 0 (width * height * 4)
  
  pure $ encodeDynamicPng $ ImageRGBA8 image

readImageFromTextureAlloc :: GL.TextureObject -> IO (Either String BL.ByteString)
readImageFromTextureAlloc tex = do
  GL.textureBinding GL.Texture2D $= Just tex
  GL.TextureSize2D width height <- GL.textureSize2D GL.Texture2D 0
  let w = fromIntegral width
      h = fromIntegral height
  pixels <- allocArray w h

  A.withStorableArray pixels $ \ptr -> do
    GL.getTexImage GL.Texture2D 0 (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
    fptr <- newForeignPtr_ ptr
    pure $ encodeDynamicPng $ ImageRGBA8 $ Image w h $ V.unsafeFromForeignPtr fptr 0 (w * h * 4)

writeImageToTexture :: GL.TextureObject -> B.ByteString -> IO ()
writeImageToTexture tex pngData = case convertRGBA8 <$> decodePng pngData of
  Left err -> error err
  Right rgba -> do
    GL.textureBinding GL.Texture2D $= Just tex
    
    V.unsafeWith (imageData rgba) $ \ptr -> do
      GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8
        (GL.TextureSize2D (fromIntegral $ imageWidth rgba) (fromIntegral $ imageHeight rgba))
        0
        (GL.PixelData GL.RGBA GL.UnsignedByte ptr)

-- TODO: embed basePath directory in binary using TH
resolveGLSL :: T.Text -> IO Text
resolveGLSL glsl = flip ST.evalStateT mempty $ do
  T.unlines <$> sequence
    [ case strip line of
        Just include -> rsv (T.unpack include)
        Nothing      -> pure line
    | line <- T.lines glsl
    ]
  where
    strip line = T.stripPrefix "#include \"" line >>= T.stripSuffix "\""

    rsv path = do
      included <- ST.get

      if S.member path included
        then pure mempty
        else do
          ST.modify (S.insert path)

          file <- liftIO $ T.readFile path
    
          T.unlines <$> sequence
            [ case strip line of
                Just include -> rsv $ Path.takeDirectory path </> T.unpack include
                Nothing      -> pure line
            | line <- T.lines file
            ]

createShader :: Maybe Text -> Text -> IO (ShaderAttribs, IO (), IO ())
createShader vertT fragT = do
  vertShader <- GL.createShader GL.VertexShader
  vertT' <- resolveGLSL $ fromMaybe defaultVertT vertT
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
    r <- GL.programInfoLog program
    error $ "createShader: " <> r

  a_pos <- get $ GL.attribLocation program "a_pos"
  a_uv <- if uv then fmap Just $ get $ GL.attribLocation program "a_uv" else pure Nothing

  return (ShaderAttribs { saPos = a_pos, saUv = a_uv, saProgram = program }, bind program, destroy vertShader fragShader program)

  where
    uv = False -- TODO: keep around for now

    defaultVertT = [i|
in vec3 a_pos;

void main() {
  gl_Position = vec4(a_pos, 1.0);
} |]

    bind program = GL.currentProgram $= Just program

    destroy vertShader fragShader program = do
      GL.deleteObjectName vertShader
      GL.deleteObjectName fragShader
      GL.deleteObjectName program

formatUniforms :: [(Text, Text)] -> Text
formatUniforms uniforms = T.intercalate "\n"
  [ [i|uniform #{ut} #{un};|]
  | (ut, un) <- uniforms
  ]
