{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module GPT where

import Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as ST
import qualified Control.Monad.Trans.Writer.CPS as W

import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Graphics.Rendering.OpenGL as GL

import GHC.Generics (Generic)

import Common
import Syn
import Syn.Run
import Types

data DefaultParams m = DefaultParams
  { u_resolution :: P m (GL.Vector2 Float)
  , u_time :: P m Float
  } deriving Generic

instance ShaderParam (DefaultParams Values)
instance NamedShaderParam DefaultParams

gptShader :: RectBuffer -> OpOptions -> Text -> IO (Op (DefaultParams Values))
gptShader rectBuf opts fragT = do
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

gptShaderSyn :: MonadIO m => RectBuffer -> OpOptions -> Text -> Signal (DefaultParams Values) -> Syn [Out] m a
gptShaderSyn rectBuf opts fragT params = do
  Op tex render destroy <- unsafeNonBlockingIO $ gptShader rectBuf opts fragT

  finalize (liftIO destroy) $ view $ pure $ Out
    { outTex = tex
    , outRender = signalValue params >>= render
    }
