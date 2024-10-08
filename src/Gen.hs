{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Gen where

import Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader

import Data.Foldable (asum)
import Data.Text (Text)

import Common
import Syn
import Types

import GHC.Generics
import GHC.TypeLits


shader0
  :: ShaderParams params
  => ShaderParamDeriveOpts
  -> (OpOptions -> ParamFields params -> Text)
  -> params
  -> Op a
shader0 deriveOpts fragT params = Op $ do
  OpContext opts rectBuf <- lift ask

  (out, destroy) <- unsafeNonBlockingIO $ do
    (tex, bindFBO, destroyFBO) <- createFramebuffer opts
    let (fields, initUniforms) = shaderParams deriveOpts
    (attribs, bindShader, destroyShader) <- createShader Nothing (fragT opts fields)

    bindShader
    setUniforms <- initUniforms (saProgram attribs)

    (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

    pure
      ( Out
          { outTex = tex
          , outRender = do
              bindFBO
              bindShader
              setUniforms params
              drawRect
          }
      , do
          destroyFBO
          destroyShader
          destroyDrawRect
      )

  finalize (liftIO destroy) $ view [out]

data Syn1 = Syn1 { tex0 :: Signal (Texture 0) }
  deriving Generic

shader1
  :: ShaderParams params
  => ShaderParamDeriveOpts
  -> (OpOptions -> ParamFields params -> ParamFields Syn1 -> Text)
  -> params
  -> Op a
  -> Op a
shader1 deriveOpts fragT params op1 = Op $ do
  OpContext opts rectBuf <- lift ask

  (f, destroy) <- unsafeNonBlockingIO $ do
    (tex, bindFBO, destroyFBO) <- createFramebuffer opts

    let (fields, initUniforms) = shaderParams deriveOpts
    let (fieldsSrc, initUniformsSrc) = shaderParams deriveOpts

    (attribs, bindShader, destroyShader) <- createShader Nothing (fragT opts fields fieldsSrc)

    bindShader
    setUniforms <- initUniforms (saProgram attribs)
    setUniformsSrc <- initUniformsSrc (saProgram attribs)

    (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

    pure
      ( \[out] -> Out
          { outTex = tex
          , outRender = do
              outRender out

              bindFBO
              bindShader

              setUniforms params
              setUniformsSrc $ Syn1 { tex0 = pure $ Texture (Just $ outTex out) }

              drawRect
          }
      , do
          destroyFBO
          destroyShader
          destroyDrawRect
      )

  finalize (liftIO destroy) $ mapView (pure . f) (runOp op1)

data Syn2 = Syn2 { tex0 :: Signal (Texture 0), tex1 :: Signal (Texture 1) }
  deriving Generic

shader2
  :: ShaderParams params
  => ShaderParamDeriveOpts
  -> (OpOptions -> ParamFields params -> ParamFields Syn2 -> Text)
  -> params
  -> Op a
  -> Op a
  -> Op a
shader2 deriveOpts fragT params op1 op2 = Op $ do
  OpContext opts rectBuf <- lift ask

  (f, destroy) <- unsafeNonBlockingIO $ do
    (tex, bindFBO, destroyFBO) <- createFramebuffer opts

    let (fields, initUniforms) = shaderParams deriveOpts
    let (fieldsSrc, initUniformsSrc) = shaderParams deriveOpts

    (attribs, bindShader, destroyShader) <- createShader Nothing (fragT opts fields fieldsSrc)

    bindShader
    setUniforms <- initUniforms (saProgram attribs)
    setUniformsSrc <- initUniformsSrc (saProgram attribs)

    (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

    pure
      ( \[out1, out2] -> Out
          { outTex = tex
          , outRender = do
              outRender out1
              outRender out2

              bindFBO
              bindShader

              setUniforms params
              setUniformsSrc $ Syn2
                { tex0 = pure $ Texture (Just $ outTex out1)
                , tex1 = pure $ Texture (Just $ outTex out2)
                }

              drawRect
          }
      , do
          destroyFBO
          destroyShader
          destroyDrawRect
      )

  finalize (liftIO destroy) $ mapView (pure . f) $ asum [ runOp op1, runOp op2 ]
