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

import Data.Foldable (asum)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Graphics.Rendering.OpenGL as GL

import Common
import Syn
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

genShader1
  :: FromTuples tuples (HList params)
  => Params params

  => RectBuffer
  -> OpOptions
  -> ShaderParamDeriveOpts
  -> tuples
  -> (ParamFields (Param "tex0" (Texture 0) ': params) -> Text)
  -> IO (GenOp (Param "tex0" (Texture 0) ': params))
genShader1 rectBuf opts deriveOpts tuples fragT = do
  (tex, bindFBO, destroyFBO) <- createFramebuffer opts
  let params = param #tex0 (texture @0 Nothing) :. fromTuples tuples
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

genShader2
  :: FromTuples tuples (HList params)
  => Params params

  => RectBuffer
  -> OpOptions
  -> ShaderParamDeriveOpts
  -> tuples
  -> (ParamFields (Param "tex0" (Texture 0) ': Param "tex1" (Texture 1) ': params) -> Text)
  -> IO (GenOp (Param "tex0" (Texture 0) ': Param "tex1" (Texture 1) ': params))
genShader2 rectBuf opts deriveOpts tuples fragT = do
  (tex, bindFBO, destroyFBO) <- createFramebuffer opts
  let params = param #tex0 (texture @0 Nothing) :. param #tex1 (texture @1 Nothing) :. fromTuples tuples
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

genShaderSyn1
  :: MonadIO m
  => FromTuples tuples (HList params)
  => Params params
  => RectBuffer
  -> OpOptions
  -> ShaderParamDeriveOpts
  -> tuples
  -> (ParamFields (Param "tex0" (Texture 0) ': params) -> Text)
  -> GenSignal params
  -> Syn [Out] m a
  -> Syn [Out] m a
genShaderSyn1 rectBuf opts deriveOpts tuples fragT (GenSignal signal) op0 = do
  GenOp tex render destroy <- unsafeNonBlockingIO $ genShader1 rectBuf opts deriveOpts tuples fragT
  finalize (liftIO destroy) $ mapView (pure . f tex render) op0
  where
    f tex render [out] = Out
      { outTex = tex
      , outRender = do
          outRender out
          (fmap (param #tex0 (texture @0 (Just $ outTex out)) :.) $ seqParams (fromTuples signal)) >>= render
      }

genShaderSyn2
  :: MonadIO m
  => FromTuples tuples (HList params)
  => Params params
  => RectBuffer
  -> OpOptions
  -> ShaderParamDeriveOpts
  -> tuples
  -> (ParamFields (Param "tex0" (Texture 0) ': Param "tex1" (Texture 1) ': params) -> Text)
  -> GenSignal params
  -> Syn [Out] m a
  -> Syn [Out] m a
  -> Syn [Out] m a
genShaderSyn2 rectBuf opts deriveOpts tuples fragT (GenSignal signal) op0 op1 = do
  GenOp tex render destroy <- unsafeNonBlockingIO $ genShader2 rectBuf opts deriveOpts tuples fragT
  finalize (liftIO destroy) $ mapView (pure . f tex render) $ asum [ op0, op1 ]
  where
    g out1 out2 p = param #tex0 (texture @0 (Just $ outTex out1)) :. param #tex1 (texture @1 (Just $ outTex out2)) :. p

    f tex render [out1, out2] = Out
      { outTex = tex
      , outRender = do
          outRender out1
          outRender out2
          fmap (g out1 out2) (seqParams (fromTuples signal)) >>= render
      }
