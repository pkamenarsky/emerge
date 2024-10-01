{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Render where

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

import Common
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

data BlitParams = BlitParams
  { blitSource :: Texture 0
  } deriving Generic

instance ShaderParam BlitParams where

blit :: RectBuffer -> GL.Size -> IO (GL.TextureObject -> IO (), IO ())
blit rectBuf viewport = do
  (attribs, bindShader, destroyShader) <- createShader vertT fragT True
  (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs
  setParams <- shaderParam defaultShaderParamDeriveOpts (saProgram attribs)

  pure
    ( \tex -> do
        GL.viewport $= (GL.Position 0 0, viewport)
        GL.clearColor $= GL.Color4 0 0 0 0

        GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject

        bindShader
        setParams $ BlitParams (Texture tex)
        
        drawRect

    , do
        destroyShader
        destroyDrawRect
    ) 
  where
    vertT = [i|
in vec3 a_pos;
in vec2 a_uv;

varying vec2 uv;

void main() {
    uv = a_uv;
    gl_Position = vec4(a_pos, 1.0);
} |]
      
    fragT = [i|
in vec2 uv;

uniform sampler2D blitSource;

void main() {
  gl_FragColor = texture2D(blitSource, uv);
} |]

-- Ops (fill) ------------------------------------------------------------------

data FillParams = FillParams
  { foColor :: GL.Color4 Float
  } deriving Generic

instance ShaderParam FillParams where

fill :: RectBuffer -> OpOptions -> IO (Op FillParams)
fill rectBuf opts = do
  (tex, bindFBO, destroyFBO) <- createFramebuffer opts
  (attribs, bindShader, destroyShader) <- createShader vertT fragT False
  setParams <- shaderParam defaultShaderParamDeriveOpts (saProgram attribs)

  (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

  pure $ Op
    { opTex = tex
    , opRender = \params -> do
        bindFBO
        bindShader
        setParams params
        drawRect
    , opDestroy = do
        destroyFBO
        destroyShader
        destroyDrawRect
    }
  where
    vertT = [i|
in vec3 a_pos;

void main() {
    gl_Position = vec4(a_pos, 1.0);
} |]

    fragT = [i|
uniform vec4 foColor;

void main() {
  gl_FragColor = foColor;
} |]

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
  setParams <- shaderParam defaultShaderParamDeriveOpts (saProgram attribs)

  (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

  pure $ Op
    { opTex = tex
    , opRender = \params -> do
        bindFBO
        bindShader
        setParams params
        drawRect
    , opDestroy = do
        destroyFBO
        destroyShader
        destroyDrawRect
    }
  where
    vertT = [i|
in vec3 a_pos;
in vec2 a_uv;

varying vec2 uv;

void main() {
  gl_Position = vec4(a_pos, 1.0);
  uv = a_uv;
} |]

    fragT = [i|
in vec2 uv;

uniform vec4 cpColor;
uniform vec2 cpCenter;
uniform float cpRadius;

void main() {
  float dist = distance(uv, cpCenter);
  float delta = fwidth(dist);
  float alpha = smoothstep(cpRadius - delta, cpRadius, dist);

  gl_FragColor = cpColor * (1. - alpha);
} |]

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
  , bpTex1 :: Texture 0
  , bpTex2 :: Texture 1
  } deriving Generic

instance ShaderParam BlendParams where

blend :: RectBuffer -> BlendOptions -> IO (Op BlendParams)
blend rectBuf opts = do
  (tex, bindFBO, destroyFBO) <- createFramebuffer (bpOpOptions opts)
  (attribs, bindShader, destroyShader) <- createShader vertT fragT True
  setParams <- shaderParam defaultShaderParamDeriveOpts (saProgram attribs)

  (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

  pure $ Op
    { opTex = tex
    , opRender = \params -> do
        bindFBO
        bindShader
        setParams params
        drawRect
    , opDestroy = do
        destroyFBO
        destroyShader
        destroyDrawRect
    }
  where
    vertT = [i|
in vec3 a_pos;
in vec2 a_uv;

varying vec2 uv;

void main() {
    uv = a_uv;

    gl_Position = vec4(a_pos, 1.0);
} |]

    fragT = [i|
in vec2 uv;

uniform sampler2D bpTex1;
uniform sampler2D bpTex2;
uniform float bpFactor;

void main() {
  #{mode}
} |]

    mode :: T.Text
    mode = case bpMode opts of
      Add -> "  gl_FragColor = texture2D(bpTex1, uv) * bpFactor + texture2D(bpTex2, uv) * (1. - bpFactor);"
      Mul -> "  gl_FragColor = texture2D(bpTex1, uv) * texture2D(bpTex2, uv);"

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
              , bpTex1 = Texture $ outTex aOut
              , bpTex2 = Texture $ outTex bOut
              }
        }
      out l = pure $ Out
        { outTex = tex
        , outRender = print (length l) >> pure ()
        }

  finalize (liftIO destroy) $ mapView out $ asum [ a, b ]
