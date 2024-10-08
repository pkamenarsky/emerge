{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
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

blit :: GL.BufferObject -> GL.Size -> IO (GL.TextureObject -> IO (), IO ())
blit rectBuf viewport@(GL.Size width height) = do
  (attribs, bindShader, destroyShader) <- createShader Nothing fragT
  (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

  bindShader
  loc <- GL.uniformLocation (saProgram attribs) "tex"

  pure
    ( \tex -> do
        GL.viewport $= (GL.Position 0 0, viewport)
        GL.clearColor $= GL.Color4 0 0 0 0

        GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject

        bindShader
        
        GL.activeTexture $= GL.TextureUnit 0
        GL.textureBinding GL.Texture2D $= Just tex
        GL.uniform loc $= GL.TextureUnit 0
        
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
  gl_FragColor = texture2D(tex, uv);
} |]

-- Ops (fill) ------------------------------------------------------------------

data FillUniforms = FillUniforms
  { color :: Signal Color4
  } deriving Generic

instance Default FillUniforms where
  def = FillUniforms { color = pure $ color4 1 1 1 1 }

fill :: FillUniforms -> Op a
fill = shader0 x fragT
  where
    fragT _ u = [i|
uniform vec4 foColor;

void main() {
  gl_FragColor = #{uniform u #color};
} |]

-- Ops (circle) ----------------------------------------------------------------

data CircleUniforms = CircleUniforms
  { radius :: Signal Float
  , color :: Signal Color4
  , center :: Signal Vec2
  } deriving Generic

instance Default CircleUniforms where
  def = CircleUniforms
    { radius = pure $ float 0.5
    , color = pure $ color4 1 1 1 1
    , center = pure $ vec2 0.5 0.5
    }

circle :: CircleUniforms -> Op a
circle = shader0 x fragT
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

-- Ops (blend) -----------------------------------------------------------------

data BlendMode = Add | Mul

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
blend opts = shader2 x fragT
  where
    modeFrag u src = t $ case mode opts of
      Add -> [i|  gl_FragColor = texture2D(#{uniform src #tex0}, uv) * #{uniform u #factor} + texture2D(#{uniform src #tex1}, uv) * (1. - #{uniform u #factor});|]
      Mul -> [i|  gl_FragColor = texture2D(#{uniform src #tex0}, uv) * #{uniform u #factor} * texture2D(#{uniform src #tex1}, uv) * (1. - #{uniform u #factor});|]

    fragT opOpts u src = [i|
#{formatParamUniforms u}
#{formatParamUniforms src}

void main() {
  vec2 uv = gl_FragCoord.xy / #{resVec2 opOpts};

  #{modeFrag u src}
} |]

    t :: Text -> Text
    t = id

--------------------------------------------------------------------------------

xform :: MonadIO m => GL.BufferObject -> OpOptions -> (BL.ByteString -> IO B.ByteString) -> Syn [Out] m a -> Syn [Out] m a
xform rectBuf opts io s = do
  (tex', bindFBO, bindShader, uniforms) <- unsafeNonBlockingIO $ do
    (tex', bindFBO, destroyFBO) <- createFramebuffer opts
    let (udefs, getUniforms) = shaderParams'' x $ #tex =: texture @0 (Just tex')
    (attribs, bindShader, destroyShader) <- createShader Nothing (fragT udefs)

    bindShader
    uniforms <- getUniforms (saProgram attribs)

    pure (tex', bindFBO, bindShader, uniforms)
  
  -- TODO finalize
  finalize (pure ()) $ mapView (f tex' bindFBO bindShader uniforms) s
  where
    fragT udefs = [i|
#{formatParamUniforms udefs}

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
                 set uniforms $ fromTuples $ #tex =: texture @0 (Just tex')
                 writeImageToTexture tex' image
               Left e -> error e
         }
      ]
          
