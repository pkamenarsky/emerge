{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Proxy
import Data.StateVar

import qualified Graphics.Rendering.OpenGL as GL

import GHC.Generics (Generic, Rep, V1, U1, (:*:) ((:*:)), C, D, K1 (K1), M1 (M1), S, Meta (MetaSel), from)
import GHC.TypeLits

--------------------------------------------------------------------------------

newtype Frame = Frame Int
  deriving (Eq, Ord)

--------------------------------------------------------------------------------

data ShaderParamDeriveOpts = ShaderParamDeriveOpts
  { spFieldLabelModifier :: String -> String
  }

defaultShaderParamDeriveOpts :: ShaderParamDeriveOpts
defaultShaderParamDeriveOpts = ShaderParamDeriveOpts id

class GShaderParam f where                                                           
  gShaderParam :: ShaderParamDeriveOpts -> GL.Program -> IO (f a -> IO ())

instance GShaderParam V1 where gShaderParam _ _ = pure $ \_ -> pure ()
instance GShaderParam U1 where gShaderParam _ _ = pure $ \_ -> pure ()

instance (GShaderParam a, GShaderParam b) => GShaderParam (a :*: b) where
  gShaderParam opts program = do
    setA <- gShaderParam opts program
    setB <- gShaderParam opts program

    pure $ \(a :*: b) -> setA a >> setB b

instance GShaderParam a => GShaderParam (M1 C i a) where
  gShaderParam opts program = do
    set <- gShaderParam opts program
    pure $ \(M1 a) -> set a

instance GShaderParam a => GShaderParam (M1 D i a) where
  gShaderParam opts program = do
    set <- gShaderParam opts program
    pure $ \(M1 a) -> set a

instance (KnownSymbol name, GL.Uniform a) => GShaderParam (M1 S ('MetaSel ('Just name) u s t) (K1 i a)) where
  gShaderParam opts program = do
    loc <- GL.uniformLocation program (spFieldLabelModifier opts $ symbolVal nameP)
    pure $ \(M1 (K1 a)) -> GL.uniform loc $= a
    where
      nameP :: Proxy name
      nameP = Proxy

class ShaderParam a where
  shaderParam :: GL.Program -> IO (a -> IO ())

genericShaderParam :: Generic a => GShaderParam (Rep a) => ShaderParamDeriveOpts -> GL.Program -> IO (a -> IO ())
genericShaderParam opts program = do
  set <- gShaderParam opts program
  pure $ \a -> set (from a)
