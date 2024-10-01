{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Common where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.State as ST

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Foldable
import Data.IORef
import Data.ObjectName
import Data.Machine.MealyT
import Data.StateVar
import qualified Data.Set as S
import Data.String.Interpolate (i)
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

--------------------------------------------------------------------------------

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

  pure (tex, bind fbo, destroy fbo tex)

  where
    bind fbo = do
      GL.bindFramebuffer GL.Framebuffer $= fbo
      GL.viewport $= (GL.Position 0 0, GL.Size (opWidth opts) (opHeight opts))

    destroy fbo tex = do
      deleteObjectName fbo
      deleteObjectName tex

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

  set <- shaderParam defaultShaderParamDeriveOpts program

  return (ShaderAttribs { saPos = a_pos, saUv = a_uv, saProgram = program }, bind program set, destroy vertShader fragShader program)
    where
      bind program setParams params = do
        GL.currentProgram $= Just program
        setParams params

      destroy vertShader fragShader program = do
        GL.deleteObjectName vertShader
        GL.deleteObjectName fragShader
        GL.deleteObjectName program
