{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Gen where

import Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as ST
import qualified Control.Monad.Trans.Writer.CPS as W

import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Graphics.Rendering.OpenGL as GL

import Common
import Syn
import Syn.Run
import Types

data GenOp params = GenOp
  { gopTex :: GL.TextureObject
  , gopRender :: forall subparams. Params subparams => SubSet subparams params ~ 'True => HList subparams -> IO ()
  , gopDestroy :: IO ()
  }

genShader
  :: FromTuples tuples (HList params)
  => Params params

  => RectBuffer
  -> OpOptions
  -> ShaderParamDeriveOpts
  -> tuples
  -> (ParamFields params -> Text)
  -> IO (GenOp params)
genShader rectBuf opts deriveOpts tuples fragT = do
  (tex, bindFBO, destroyFBO) <- createFramebuffer opts
  let params = fromTuples tuples
  let (udefs, initUniforms) = shaderParams' deriveOpts params
  (attribs, bindShader, destroyShader) <- createShader Nothing (fragT udefs)

  bindShader
  uniforms <- initUniforms (saProgram attribs)

  (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

  pure $ GenOp
    { gopTex = tex
    , gopRender = \params -> do
        bindFBO
        bindShader
        set uniforms params
        drawRect
    , gopDestroy = do
        destroyFBO
        destroyShader
        destroyDrawRect
    }

data GenSignal params = forall subtuples subparams subsigparams. (FromTuples subtuples (HList subsigparams), SeqParams subsigparams subparams, Params subparams, SubSet subparams params ~ 'True) => GenSignal subtuples

genShaderSyn
  :: MonadIO m
  => FromTuples tuples (HList params)
  => Params params
  => RectBuffer
  -> OpOptions
  -> ShaderParamDeriveOpts
  -> tuples
  -> (ParamFields params -> Text)
  -> GenSignal params
  -> Syn [Out] m a
genShaderSyn rectBuf opts deriveOpts tuples fragT (GenSignal signal) = do
  GenOp tex render destroy <- unsafeNonBlockingIO $ genShader rectBuf opts deriveOpts tuples fragT

  finalize (liftIO destroy) $ view $ pure $ Out
    { outTex = tex
    , outRender = seqParams (fromTuples signal) >>= render
    }
