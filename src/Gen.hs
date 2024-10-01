{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Gen where

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
  } deriving Generic

instance ShaderParam (DefaultParams Values)
instance NamedShaderParam (DefaultParams Fields)

genShader :: ShaderParam (params Values) => NamedShaderParam (params Fields) => RectBuffer -> OpOptions -> Text -> IO (Op (params Values))
genShader rectBuf opts fragT = do
  (tex, bindFBO, destroyFBO) <- createFramebuffer opts
  (attribs, bindShader, destroyShader) <- createShader Nothing fragT

  setDefaultParams <- shaderParam defaultShaderParamDeriveOpts (saProgram attribs)
  setParams <- shaderParam defaultShaderParamDeriveOpts (saProgram attribs)

  (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

  pure $ Op
    { opTex = tex
    , opRender = \params -> do
        bindFBO
        bindShader
        setDefaultParams $ DefaultParams @Values $ GL.Vector2 (fi $ opWidth opts) (fi $ opHeight opts)
        setParams params
        drawRect
    , opDestroy = do
        destroyFBO
        destroyShader
        destroyDrawRect
    }

genShaderSyn :: MonadIO m => RectBuffer -> OpOptions -> Text -> Signal (DefaultParams Values) -> Syn [Out] m a
genShaderSyn rectBuf opts fragT params = do
  Op tex render destroy <- unsafeNonBlockingIO $ genShader rectBuf opts fragT

  finalize (liftIO destroy) $ view $ pure $ Out
    { outTex = tex
    , outRender = signalValue params >>= render
    }
