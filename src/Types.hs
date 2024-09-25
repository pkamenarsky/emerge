{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DeriveGeneric #-}

module Types where

import Control.Monad (when)

import Data.Proxy
import Data.StateVar

import qualified Graphics.Rendering.OpenGL as GL

import GHC.Generics (Generic, Rep, V1, U1, (:*:) ((:*:)), C, D, K1 (K1), M1 (M1), S, Meta (MetaSel), from)
import GHC.TypeLits

--------------------------------------------------------------------------------

newtype Frame = Frame Int
  deriving (Eq, Ord)

--------------------------------------------------------------------------------

newtype Tex1 = Tex1 GL.TextureObject
newtype Tex2 = Tex2 GL.TextureObject
newtype Tex3 = Tex3 GL.TextureObject

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

instance {-# OVERLAPPABLE #-} (KnownSymbol name, GL.Uniform a) => GShaderParam (M1 S ('MetaSel ('Just name) u s t) (K1 i a)) where
  gShaderParam opts program = do
    let uName = spFieldLabelModifier opts $ symbolVal nameP

    loc <- GL.uniformLocation program uName
    
    when (loc < GL.UniformLocation 0) $ error $ "gShaderParam: uniform " <> uName <> " not found"

    pure $ \(M1 (K1 a)) -> GL.uniform loc $= a
    where
      nameP :: Proxy name
      nameP = Proxy

instance {-# OVERLAPS #-} KnownSymbol name => GShaderParam (M1 S ('MetaSel ('Just name) u s t) (K1 i Tex1)) where
  gShaderParam opts program = do
    let uName = spFieldLabelModifier opts $ symbolVal nameP

    loc <- GL.uniformLocation program uName
    
    when (loc < GL.UniformLocation 0) $ error $ "gShaderParam: uniform " <> uName <> " not found"

    pure $ \(M1 (K1 (Tex1 tex))) -> do
      GL.activeTexture $= GL.TextureUnit 0
      GL.textureBinding GL.Texture2D $= Just tex
      GL.uniform loc $= GL.TextureUnit 0
    where
      nameP :: Proxy name
      nameP = Proxy

instance {-# OVERLAPS #-} KnownSymbol name => GShaderParam (M1 S ('MetaSel ('Just name) u s t) (K1 i Tex2)) where
  gShaderParam opts program = do
    let uName = spFieldLabelModifier opts $ symbolVal nameP

    loc <- GL.uniformLocation program uName
    
    when (loc < GL.UniformLocation 0) $ error $ "gShaderParam: uniform " <> uName <> " not found"

    pure $ \(M1 (K1 (Tex2 tex))) -> do
      GL.activeTexture $= GL.TextureUnit 1
      GL.textureBinding GL.Texture2D $= Just tex
      GL.uniform loc $= GL.TextureUnit 1
    where
      nameP :: Proxy name
      nameP = Proxy

instance {-# OVERLAPPING #-} KnownSymbol name => GShaderParam (M1 S ('MetaSel ('Just name) u s t) (K1 i Tex3)) where
  gShaderParam opts program = do
    let uName = spFieldLabelModifier opts $ symbolVal nameP

    loc <- GL.uniformLocation program uName
    
    when (loc < GL.UniformLocation 0) $ error $ "gShaderParam: uniform " <> uName <> " not found"

    pure $ \(M1 (K1 (Tex3 tex))) -> do
      GL.activeTexture $= GL.TextureUnit 2
      GL.textureBinding GL.Texture2D $= Just tex
      GL.uniform loc $= GL.TextureUnit 2
    where
      nameP :: Proxy name
      nameP = Proxy

class ShaderParam a where
  shaderParam :: ShaderParamDeriveOpts -> GL.Program -> IO (a -> IO ())
  default shaderParam :: Generic a => GShaderParam (Rep a) => ShaderParamDeriveOpts -> GL.Program -> IO (a -> IO ())
  shaderParam opts program = do
    set <- gShaderParam opts program
    pure $ \a -> set (from a)

-- For shaders without uniforms
instance ShaderParam ()

genericShaderParam :: Generic a => GShaderParam (Rep a) => ShaderParamDeriveOpts -> GL.Program -> IO (a -> IO ())
genericShaderParam opts program = do
  set <- gShaderParam opts program
  pure $ \a -> set (from a)
