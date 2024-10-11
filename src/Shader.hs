{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Shader where

import Control.Monad (when)
import Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader

import Data.Foldable (asum)
import Data.StateVar (($=))
import Data.Text (Text, pack)

import qualified Graphics.Rendering.OpenGL as GL

import Common
import Syn
import Types

--------------------------------------------------------------------------------

shader0
  :: ShaderParams params
  => ShaderParamDeriveOpts
  -> (OpOptions -> ParamFields params -> Text)
  -> params
  -> Op a
shader0 deriveOpts fragT params = Op $ do
  OpContext opts rectBuf _ _ <- lift ask

  (out, destroy) <- unsafeNonBlockingIO $ do
    (tex, bindFBO, destroyFBO) <- createFramebuffer opts
    let (fields, initUniforms) = shaderParams deriveOpts params
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
              setUniforms
              drawRect
          }
      , do
          destroyFBO
          destroyShader
          destroyDrawRect
      )

  finalize (liftIO destroy) $ view [out]

shader1
  :: ShaderParams params
  => ShaderParamDeriveOpts
  -> (OpOptions -> ParamFields params -> Text -> Text)
  -> params
  -> Op a
  -> Op a
shader1 deriveOpts fragT params op0 = Op $ do
  OpContext opts rectBuf _ _ <- lift ask

  let tex0u = "tex0"
      tex0 = spFieldLabelModifier deriveOpts tex0u

  (f, destroy) <- unsafeNonBlockingIO $ do
    (tex, bindFBO, destroyFBO) <- createFramebuffer opts

    let (ParamFields deriveOpts' fields, initUniforms) = shaderParams deriveOpts params

    (attribs, bindShader, destroyShader) <- createShader Nothing $ fragT
      opts
      (ParamFields deriveOpts' ((pack "sampler2D", pack tex0u):fields))
      (pack tex0)

    bindShader

    loc0 <- GL.uniformLocation (saProgram attribs) tex0
    when (loc0 < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> tex0 <> " not found"

    setUniforms <- initUniforms (saProgram attribs)

    (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

    pure
      ( \[out] -> Out
          { outTex = tex
          , outRender = do
              outRender out

              bindFBO
              bindShader

              GL.uniform loc0 $= TexUniform @0 (Just (outTex out))

              setUniforms

              drawRect
          }
      , do
          destroyFBO
          destroyShader
          destroyDrawRect
      )

  finalize (liftIO destroy) $ mapView (pure . f) (runOp op0)

shader2
  :: ShaderParams params
  => ShaderParamDeriveOpts
  -> (OpOptions -> ParamFields params -> Text -> Text -> Text)
  -> params
  -> Op a
  -> Op a
  -> Op a
shader2 deriveOpts fragT params op0 op1 = Op $ do
  OpContext opts rectBuf _ _ <- lift ask

  let tex0u = "tex0"
      tex0 = spFieldLabelModifier deriveOpts tex0u
      tex1u = "tex1"
      tex1 = spFieldLabelModifier deriveOpts tex1u

  (f, destroy) <- unsafeNonBlockingIO $ do
    (tex, bindFBO, destroyFBO) <- createFramebuffer opts

    let (ParamFields deriveOpts' fields, initUniforms) = shaderParams deriveOpts params

    (attribs, bindShader, destroyShader) <- createShader Nothing $ fragT
      opts
      (ParamFields deriveOpts' ((pack "sampler2D", pack tex0u):(pack "sampler2D", pack tex1u):fields))
      (pack tex0)
      (pack tex1)

    bindShader

    loc0 <- GL.uniformLocation (saProgram attribs) tex0
    loc1 <- GL.uniformLocation (saProgram attribs) tex0

    when (loc0 < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> tex0 <> " not found"
    when (loc1 < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> tex1 <> " not found"

    setUniforms <- initUniforms (saProgram attribs)

    (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

    pure
      ( \[out0, out1] -> Out
          { outTex = tex
          , outRender = do
              outRender out0
              outRender out1

              bindFBO
              bindShader

              GL.uniform loc0 $= TexUniform @0 (Just (outTex out0))
              GL.uniform loc1 $= TexUniform @1 (Just (outTex out1))

              setUniforms

              drawRect
          }
      , do
          destroyFBO
          destroyShader
          destroyDrawRect
      )

  finalize (liftIO destroy) $ mapView (pure . f) $ asum [ runOp op0, runOp op1 ]
