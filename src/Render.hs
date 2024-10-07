{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Render where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.State as ST

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
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
import Gen

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

instance ShaderParams BlitParams where

blit :: RectBuffer -> GL.Size -> IO (GL.TextureObject -> IO (), IO ())
blit rectBuf viewport@(GL.Size width height) = do
  (attribs, bindShader, destroyShader) <- createShader Nothing fragT
  (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs
  setParams <- shaderParams defaultShaderParamDeriveOpts (saProgram attribs)

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
    fragT = [i|
uniform sampler2D blitSource;

void main() {
  vec2 uv = gl_FragCoord.xy / vec2(#{width}, #{height});
  gl_FragColor = texture2D(blitSource, uv);
} |]

-- Ops (fill) ------------------------------------------------------------------

data FillParams = FillParams
  { foColor :: GL.Color4 Float
  } deriving Generic

instance ShaderParams FillParams where

fill :: RectBuffer -> OpOptions -> IO (Op FillParams)
fill rectBuf opts = do
  (tex, bindFBO, destroyFBO) <- createFramebuffer opts
  (attribs, bindShader, destroyShader) <- createShader Nothing fragT
  setParams <- shaderParams defaultShaderParamDeriveOpts (saProgram attribs)

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

instance ShaderParams CircleParams where

circle :: RectBuffer -> OpOptions -> IO (Op CircleParams)
circle rectBuf opts = do
  (tex, bindFBO, destroyFBO) <- createFramebuffer opts
  (attribs, bindShader, destroyShader) <- createShader Nothing fragT
  setParams <- shaderParams defaultShaderParamDeriveOpts (saProgram attribs)

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
    fragT = [i|
uniform vec4 cpColor;
uniform vec2 cpCenter;
uniform float cpRadius;

void main() {
  vec2 uv = gl_FragCoord.xy / vec2(#{opWidth opts}, #{opHeight opts});

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

circleSyn'
  :: MonadIO m
  => RectBuffer
  -> OpOptions
  -> GenSignal
       [ Param "radius" Float
       , Param "color" (GL.Color4 Float)
       , Param "center" (GL.Vector2 Float)
       ]
  -> Syn [Out] m a
circleSyn' rectBuffer opts = genShaderSyn rectBuffer opts def fragT
  where
    def =
      ( #radius =: float 0.5
      , #color  =: color4 1 1 1 1
      , #center =: vec2 0.5 0.5
      )
    fragT uniforms params = [i|

#{formatUniforms uniforms}

void main() {
  vec2 uv = gl_FragCoord.xy / #{field params #u_resolution}.xy;

  float dist = distance(uv, #{field params #center});
  float delta = fwidth(dist);
  float alpha = smoothstep(#{field params #radius} - delta, #{field params #radius}, dist);

  gl_FragColor = #{field params #color} * (1. - alpha);
} |]

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

instance ShaderParams BlendParams where

blend :: RectBuffer -> BlendOptions -> IO (Op BlendParams)
blend rectBuf opts = do
  (tex, bindFBO, destroyFBO) <- createFramebuffer (bpOpOptions opts)
  (attribs, bindShader, destroyShader) <- createShader Nothing fragT
  setParams <- shaderParams defaultShaderParamDeriveOpts (saProgram attribs)

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
    fragT = [i|
uniform sampler2D bpTex1;
uniform sampler2D bpTex2;
uniform float bpFactor;

void main() {
  vec2 uv = gl_FragCoord.xy / vec2(#{opWidth $ bpOpOptions opts}, #{opHeight $ bpOpOptions opts});

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

--------------------------------------------------------------------------------

xform :: MonadIO m => RectBuffer -> OpOptions -> (BL.ByteString -> IO B.ByteString) -> Syn [Out] m a -> Syn [Out] m a
xform rectBuf opts io s = do
  (tex', bindFBO, bindShader, uniforms) <- unsafeNonBlockingIO $ do
    (tex', bindFBO, destroyFBO) <- createFramebuffer opts
    let (fields, initUniforms) = shaderParams' $ fromTuples $ O $ #tex =: tex @0 tex'
    (attribs, bindShader, destroyShader) <- createShader Nothing (fragT fields)

    bindShader
    uniforms <- initUniforms (saProgram attribs)

    pure (tex', bindFBO, bindShader, uniforms)
  
  -- TODO finalize
  finalize (pure ()) $ mapView (f tex' bindFBO bindShader uniforms) s
  where
    fragT fields = [i|
#{formatUniforms fields}

void main() {
  vec2 uv = gl_FragCoord.xy / vec2(#{opWidth opts}, #{opHeight opts});
  gl_FragColor = texture2D(tex, uv);
} |]

    f tex' bindFBO bindShader uniforms [out] =
      [ Out
         { outTex = tex'
         , outRender = do
             outRender out
             img <- readImageFromTextureAlloc (outTex out)
             case img of
               Right bs -> do
                 image <- io bs
                 bindFBO
                 bindShader
                 set uniforms $ fromTuples $ O $ #tex =: tex @0 tex'
                 writeImageToTexture tex' image
               Left e -> error e
         }
      ]
          
