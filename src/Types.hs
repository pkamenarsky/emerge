{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DeriveGeneric #-}

module Types where

import Control.Monad (when)

import Data.Proxy
import Data.StateVar
import Data.Word (Word32)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Graphics.Rendering.OpenGL as GL

import GHC.Generics (Generic, Rep, V1, U1 (U1), (:*:) ((:*:)), C, D, K1 (K1), M1 (M1), S, Meta (MetaSel), from, to)
import GHC.TypeLits

--------------------------------------------------------------------------------

newtype Frame = Frame Int
  deriving (Eq, Ord)

--------------------------------------------------------------------------------

newtype Texture (n :: Nat) = Texture GL.TextureObject

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

instance {-# OVERLAPPING #-} (KnownSymbol name, KnownNat n) => GShaderParam (M1 S ('MetaSel ('Just name) u s t) (K1 i (Texture n))) where
  gShaderParam opts program = do
    let uName = spFieldLabelModifier opts $ symbolVal nameP

    loc <- GL.uniformLocation program uName
    
    when (loc < GL.UniformLocation 0) $ error $ "gShaderParam: uniform " <> uName <> " not found"

    pure $ \(M1 (K1 (Texture tex))) -> do
      GL.activeTexture $= GL.TextureUnit (fromInteger $ natVal unit)
      GL.textureBinding GL.Texture2D $= Just tex
      GL.uniform loc $= GL.TextureUnit (fromInteger $ natVal unit)
    where
      nameP :: Proxy name
      nameP = Proxy

      unit :: Proxy n
      unit = Proxy


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

--------------------------------------------------------------------------------

class GLSLType t where
  glslType :: Proxy t -> Text

instance GLSLType GL.GLint where
  glslType _ = "int"

-- instance GLSLType GL.GLuint where
--   glslType _ = "uint"

instance GLSLType Float where
  glslType _ = "float"

instance GLSLType Double where
  glslType _ = "double"

instance GLSLType (GL.Vector3 Float) where
  glslType _ = "vec3"

--------------------------------------------------------------------------------

data Field
data Value

type family P f v where
  P Field t = Text
  P Value t = t

--------------------------------------------------------------------------------

newtype FieldDef f = FieldDef { field :: Text }

class GNamedShaderParam g f | g -> g where                                                           
  gNamedShaderParam :: Proxy (g a) -> ShaderParamDeriveOpts -> ([(Text, Text)], f a)

instance GNamedShaderParam U1 U1 where gNamedShaderParam _ _ = ([], U1)

instance (GNamedShaderParam ga fa, GNamedShaderParam gb fb) => GNamedShaderParam (ga :*: gb) (fa :*: fb) where
  gNamedShaderParam p opts = (ap <> bp, a :*: b)
    where
      (ap, a) = gNamedShaderParam (Proxy :: Proxy (ga a)) opts
      (bp, b) = gNamedShaderParam (Proxy :: Proxy (gb a)) opts

instance GNamedShaderParam g f => GNamedShaderParam (M1 C i g) (M1 C i f) where
  gNamedShaderParam p opts = M1 <$> gNamedShaderParam (Proxy :: Proxy (g a)) opts

instance GNamedShaderParam g f => GNamedShaderParam (M1 D i g) (M1 D i f) where
  gNamedShaderParam p opts = M1 <$> gNamedShaderParam (Proxy :: Proxy (g a))  opts

instance (KnownSymbol name, GLSLType g) => GNamedShaderParam (M1 S ('MetaSel ('Just name) u s t) (K1 i g)) (M1 S ('MetaSel ('Just name) u s t) (K1 i Text)) where
  gNamedShaderParam _ opts = ([(glslType typeT, fieldName)], M1 (K1 fieldName))
    where
      fieldName = T.pack $ spFieldLabelModifier opts $ symbolVal nameP

      typeT :: Proxy g
      typeT = Proxy

      nameP :: Proxy name
      nameP = Proxy

class NamedShaderParam a where
  namedShaderParam :: ShaderParamDeriveOpts -> ([(Text, Text)], a Field)
  default namedShaderParam
    :: Generic (a Field)
    => GNamedShaderParam (Rep (a Value)) (Rep (a Field))
    => ShaderParamDeriveOpts
    -> ([(Text, Text)], a Field)
  namedShaderParam opts = fmap to $ gNamedShaderParam (Proxy :: Proxy (Rep (a Value) x)) opts
