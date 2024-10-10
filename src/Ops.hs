{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Ops where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader

import Data.IORef
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.StateVar
import Data.String.Interpolate (i)
import Data.Text (Text)

import qualified Graphics.Rendering.OpenGL as GL

import Common
import Shader
import Syn

import GHC.Generics

import Types

--------------------------------------------------------------------------------

blit :: GL.BufferObject -> GL.Size -> IO (GL.TextureObject -> IO (), IO ())
blit rectBuf viewport@(GL.Size width height) = do
  (attribs, bindShader, destroyShader) <- createShader Nothing fragT
  (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

  bindShader
  loc <- GL.uniformLocation (saProgram attribs) "tex"

  pure
    ( \tex -> do
        GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
        GL.viewport $= (GL.Position 0 0, viewport)

        bindShader
        
        GL.uniform loc $= TexUniform @0 (Just tex)
        
        drawRect

    , do
        destroyShader
        destroyDrawRect
    ) 
  where
    fragT = [i|
uniform sampler2D tex;

void main() {
  vec2 uv = gl_FragCoord.xy / vec2(#{width}, #{height});
  vec4 t = texture2D(tex, uv);
  // gl_FragColor = t;
  gl_FragColor = vec4(t.rgb * t.a, 1.);
} |]

-- feedback --------------------------------------------------------------------

feedback :: (Op a -> Op a) -> Op a
feedback x = Op $ do
  OpContext opts rectBuf <- lift ask

  ref <- unsafeNonBlockingIO $ newIORef Nothing

  (out, copy, destroy) <- unsafeNonBlockingIO $ do
    (tex, bindFBO, destroyFBO) <- createFramebuffer opts
    (attribs, bindShader, destroyShader) <- createShader Nothing (fragT opts)
    (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

    bindShader
    loc <- GL.uniformLocation (saProgram attribs) "tex"

    pure
      ( Out { outRender = pure (), outTex = tex }
      , \fixtex -> do
          bindFBO
          bindShader

          GL.uniform loc $= TexUniform @0 (Just fixtex)

          drawRect
      , do
          destroyFBO
          destroyDrawRect
          destroyShader
      )

  finalize (liftIO destroy) $ mapView (pure . f copy) $ runOp $ x $ Op $ view [out]
  where
    f copy [out] = Out
      { outRender = do
          outRender out
          copy (outTex out)
      , outTex = outTex out
      }

    fragT opts = [i|
uniform sampler2D tex;

void main() {
  vec2 uv = gl_FragCoord.xy / #{resVec2 opts};
  gl_FragColor = texture2D(tex, uv);
} |]

-- fill ------------------------------------------------------------------------

data FillUniforms = FillUniforms
  { color :: Signal Color4
  } deriving Generic

instance Default FillUniforms where
  def = FillUniforms { color = pure $ color4 1 1 1 1 }

fill :: FillUniforms -> Op a
fill = shader0 o fragT
  where
    fragT _ u = [i|
uniform vec4 foColor;

void main() {
  gl_FragColor = #{uniform u #color};
} |]

-- circle ----------------------------------------------------------------------

data CircleUniforms = CircleUniforms
  { radius :: Signal Float
  , color :: Signal Color4
  , center :: Signal Vec2
  } deriving Generic

instance Default CircleUniforms where
  def = CircleUniforms
    { radius = pure 0.5
    , color = pure $ color4 1 1 1 1
    , center = pure $ vec2 0.5 0.5
    }

circle :: CircleUniforms -> Op a
circle = shader0 o fragT
  where
    fragT opts u = [i|

#{formatParamUniforms u}

void main() {
  vec2 uv = gl_FragCoord.xy / #{resVec2 opts}.xy;

  float dist = distance(uv, #{uniform u #center});
  float delta = fwidth(dist);
  float alpha = smoothstep(#{uniform u #radius} - delta, #{uniform u #radius}, dist);

  gl_FragColor = #{uniform u #color} * (1. - alpha);
} |]

-- blend -----------------------------------------------------------------------

data BlendMode = Add | Mul | Over

data BlendOptions = BlendOptions
  { mode :: BlendMode
  }

instance Default BlendOptions where
  def = BlendOptions { mode = Add }

data BlendUniforms = BlendUniforms
  { factor :: Signal Float
  } deriving Generic

instance Default BlendUniforms where
  def = BlendUniforms { factor = pure 0.5 }

blend :: BlendOptions -> BlendUniforms -> Op a -> Op a -> Op a
blend opts = shader2 o fragT
  where
    modeFrag u tex0 tex1 = t $ case mode opts of
      Add  -> [i|  gl_FragColor = texture2D(#{tex0}, uv) * #{uniform u #factor} + texture2D(#{tex1}, uv) * (1. - #{uniform u #factor});|]
      -- TODO: this is prob not right
      Mul  -> [i|  gl_FragColor = texture2D(#{tex0}, uv) * #{uniform u #factor} * texture2D(#{tex1}, uv) * (1. - #{uniform u #factor});|]
      Over -> [i|
  vec4 t0 = texture2D(#{tex0}, uv);
  vec4 t1 = texture2D(#{tex1}, uv);

  float a = t1.a + t0.a * (1. - t1.a);
  gl_FragColor = vec4((t1.rgb * t1.a + t0.rgb * t0.a * (1. - t1.a)) / a, a);
|]

    fragT opOpts u tex0 tex1 = [i|
#{formatParamUniforms u}

void main() {
  vec2 uv = gl_FragCoord.xy / #{resVec2 opOpts};

  #{modeFrag u tex0 tex1}
} |]

    t :: Text -> Text
    t = id

-- grain -----------------------------------------------------------------------

data GrainUniforms = GrainUniforms
  { multiplier :: Signal Float
  , t :: Signal Float
  } deriving Generic

instance Default GrainUniforms where
  def = GrainUniforms
    { multiplier = pure 2.5
    , t = pure 0
    }

grain :: GrainUniforms -> Op a -> Op a
grain = shader1 o fragT
  where
    fragT opOpts u tex0 = [i|
\#include "assets/lygia/distort/grain.glsl"

#{formatParamUniforms u}

void main() {
  vec2 uv = gl_FragCoord.xy / #{resVec2 opOpts};
  gl_FragColor = vec4(grain(#{tex0}, uv, #{resVec2 opOpts}, #{uniform u #t}, #{uniform u #multiplier}), 1.);
} |]

--------------------------------------------------------------------------------

mapOp :: (BL.ByteString -> IO B.ByteString) -> Op a -> Op a
mapOp io op = Op $ do
  OpContext opts _ <- lift ask

  (f, destroy) <- unsafeNonBlockingIO $ do
    (tex, bindFBO, destroyFBO) <- createFramebuffer opts
    (attribs, bindShader, destroyShader) <- createShader Nothing (fragT opts)

    bindShader

    loc <- GL.uniformLocation (saProgram attribs) "tex"
    when (loc < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> "tex" <> " not found"

    pure 
      ( \[out] -> Out
           { outTex = tex
           , outRender = do
               outRender out
               img <- readImageFromTextureAlloc (outTex out)
               case img of
                 Right bs -> do
                   image <- io bs

                   bindFBO
                   bindShader

                   GL.uniform loc $= TexUniform @0 (Just tex)

                   writeImageToTexture tex image
                 Left e -> error e
           }
      , do
          destroyFBO
          destroyShader
      )
  
  finalize (liftIO destroy) $ mapView (pure . f) (runOp op)

  where
    fragT opts = [i|
uniform sampler2D tex;

void main() {
  vec2 uv = gl_FragCoord.xy / #{resVec2(opts)});
  gl_FragColor = texture2D(tex, uv);
} |]
