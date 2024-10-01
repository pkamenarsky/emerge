{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DeriveGeneric #-}

module Types where

import Control.Monad (when)

import Data.Proxy
import Data.StateVar
import Data.Text (Text)
import qualified Data.Text as T

import qualified Graphics.Rendering.OpenGL as GL

import GHC.Generics (Generic, Rep, V1, U1 (U1), (:*:) ((:*:)), C, D, K1 (K1), M1 (M1), S, Meta (MetaSel), from, to)
import GHC.TypeLits

--------------------------------------------------------------------------------

newtype Time = Time Float
  deriving (Eq, Ord)

--------------------------------------------------------------------------------

newtype Texture (n :: Nat) = Texture GL.TextureObject

data ShaderParamDeriveOpts = ShaderParamDeriveOpts
  { spFieldLabelModifier :: String -> String
  }

defaultShaderParamDeriveOpts :: ShaderParamDeriveOpts
defaultShaderParamDeriveOpts = ShaderParamDeriveOpts id

class GShaderParams f where                                                           
  gShaderParams :: ShaderParamDeriveOpts -> GL.Program -> IO (f a -> IO ())

instance GShaderParams V1 where gShaderParams _ _ = pure $ \_ -> pure ()
instance GShaderParams U1 where gShaderParams _ _ = pure $ \_ -> pure ()

instance (GShaderParams a, GShaderParams b) => GShaderParams (a :*: b) where
  gShaderParams opts program = do
    setA <- gShaderParams opts program
    setB <- gShaderParams opts program

    pure $ \(a :*: b) -> setA a >> setB b

instance GShaderParams a => GShaderParams (M1 C i a) where
  gShaderParams opts program = do
    set <- gShaderParams opts program
    pure $ \(M1 a) -> set a

instance GShaderParams a => GShaderParams (M1 D i a) where
  gShaderParams opts program = do
    set <- gShaderParams opts program
    pure $ \(M1 a) -> set a

instance {-# OVERLAPPABLE #-} (KnownSymbol name, GL.Uniform a) => GShaderParams (M1 S ('MetaSel ('Just name) u s t) (K1 i a)) where
  gShaderParams opts program = do
    loc <- GL.uniformLocation program n
    
    when (loc < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n <> " not found"

    pure $ \(M1 (K1 a)) -> GL.uniform loc $= a
    where
      n = spFieldLabelModifier opts $ symbolVal $ Proxy @name

instance {-# OVERLAPPING #-} (KnownSymbol name, KnownNat n) => GShaderParams (M1 S ('MetaSel ('Just name) u s t) (K1 i (Texture n))) where
  gShaderParams opts program = do
    loc <- GL.uniformLocation program n
    
    when (loc < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n <> " not found"

    pure $ \(M1 (K1 (Texture tex))) -> do
      GL.activeTexture $= GL.TextureUnit (fromInteger $ natVal $ Proxy @n)
      GL.textureBinding GL.Texture2D $= Just tex
      GL.uniform loc $= GL.TextureUnit (fromInteger $ natVal $ Proxy @n)
    where
      n = spFieldLabelModifier opts $ symbolVal $ Proxy @name

class ShaderParams a where
  shaderParams :: ShaderParamDeriveOpts -> GL.Program -> IO (a -> IO ())
  default shaderParams :: Generic a => GShaderParams (Rep a) => ShaderParamDeriveOpts -> GL.Program -> IO (a -> IO ())
  shaderParams opts program = do
    set <- gShaderParams opts program
    pure $ \a -> set (from a)

-- For shaders without uniforms
instance ShaderParams ()

genericShaderParam :: Generic a => GShaderParams (Rep a) => ShaderParamDeriveOpts -> GL.Program -> IO (a -> IO ())
genericShaderParam opts program = do
  set <- gShaderParams opts program
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

instance GLSLType (GL.Vector2 Float) where
  glslType _ = "vec2"

instance GLSLType (GL.Vector3 Float) where
  glslType _ = "vec3"

--------------------------------------------------------------------------------

newtype FieldName t = FieldName String

instance Show (FieldName t) where
  show (FieldName n) = n

data Fields
data Values

type family P f v where
  P Fields t = FieldName t
  P Values t = t

data Only t m = Only (P m t)

data Params2 t u m = Params2 (P m t) (P m u)
data Params3 t u v m = Params3 (P m t) (P m u) (P m v)
data Params4 t u v w m = Params4 (P m t) (P m u) (P m v) (P m w)

--------------------------------------------------------------------------------

class GNamedShaderParams f where                                                           
  gNamedShaderParams :: ShaderParamDeriveOpts -> ([(Text, Text)], f a)

instance GNamedShaderParams U1 where gNamedShaderParams _ = ([], U1)

instance (GNamedShaderParams g, GNamedShaderParams f) => GNamedShaderParams (g :*: f) where
  gNamedShaderParams opts = (ap <> bp, a :*: b)
    where
      (ap, a) = gNamedShaderParams opts
      (bp, b) = gNamedShaderParams opts

instance GNamedShaderParams f => GNamedShaderParams (M1 C i f) where
  gNamedShaderParams opts = M1 <$> gNamedShaderParams opts

instance GNamedShaderParams f => GNamedShaderParams (M1 D i f) where
  gNamedShaderParams opts = M1 <$> gNamedShaderParams opts

instance (KnownSymbol name, GLSLType g) => GNamedShaderParams (M1 S ('MetaSel ('Just name) u s t) (K1 i (FieldName g))) where
  gNamedShaderParams opts = ([(glslType $ Proxy @g, T.pack fieldName)], M1 (K1 $ FieldName fieldName))
    where
      fieldName = spFieldLabelModifier opts $ symbolVal $ Proxy @name

class NamedShaderParams a where
  namedShaderParams :: ShaderParamDeriveOpts -> ([(Text, Text)], a)
  default namedShaderParams :: Generic a => GNamedShaderParams (Rep a) => ShaderParamDeriveOpts -> ([(Text, Text)], a)
  namedShaderParams = fmap to . gNamedShaderParams

-- Only ------------------------------------------------------------------------

instance GL.Uniform t => ShaderParams (Only t Values) where
  shaderParams opts program = do
    loc <- GL.uniformLocation program n
    
    when (loc < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n <> " not found"

    pure $ \(Only t) -> GL.uniform loc $= t
    where
      n = spFieldLabelModifier opts "u_par"

instance GLSLType t => NamedShaderParams (Only t Fields) where
  namedShaderParams opts = ([(glslType $ Proxy @t, T.pack n)], Only $ FieldName n)
    where
      n = spFieldLabelModifier opts "u_par"

-- Param2 ----------------------------------------------------------------------

instance (GL.Uniform t, GL.Uniform u) => ShaderParams (Params2 t u Values) where
  shaderParams opts program = do
    loc0 <- GL.uniformLocation program n0
    loc1 <- GL.uniformLocation program n1
    
    when (loc0 < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n0 <> " not found"
    when (loc1 < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n1 <> " not found"

    pure $ \(Params2 t u) -> do
      GL.uniform loc0 $= t
      GL.uniform loc1 $= u
    where
      n0 = spFieldLabelModifier opts "u_par0"
      n1 = spFieldLabelModifier opts "u_par1"

instance (GLSLType t, GLSLType u) => NamedShaderParams (Params2 t u Fields) where
  namedShaderParams opts =
    ( [ (glslType $ Proxy @t, T.pack n0)
      , (glslType $ Proxy @u, T.pack n1)
      ]
    , Params2 (FieldName n0) (FieldName n1)
    )
    where
      n0 = spFieldLabelModifier opts "u_par0"
      n1 = spFieldLabelModifier opts "u_par1"

-- Param3 ----------------------------------------------------------------------

instance (GL.Uniform t, GL.Uniform u, GL.Uniform v) => ShaderParams (Params3 t u v Values) where
  shaderParams opts program = do
    loc0 <- GL.uniformLocation program n0
    loc1 <- GL.uniformLocation program n1
    loc2 <- GL.uniformLocation program n2
    
    when (loc0 < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n0 <> " not found"
    when (loc1 < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n1 <> " not found"
    when (loc2 < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n2 <> " not found"

    pure $ \(Params3 t u v) -> do
      GL.uniform loc0 $= t
      GL.uniform loc1 $= u
      GL.uniform loc2 $= v
    where
      n0 = spFieldLabelModifier opts "u_par0"
      n1 = spFieldLabelModifier opts "u_par1"
      n2 = spFieldLabelModifier opts "u_par2"

instance (GLSLType t, GLSLType u, GLSLType v) => NamedShaderParams (Params3 t u v Fields) where
  namedShaderParams opts =
    ( [ (glslType $ Proxy @t, T.pack n0)
      , (glslType $ Proxy @u, T.pack n1)
      , (glslType $ Proxy @v, T.pack n2)
      ]
    , Params3 (FieldName n0) (FieldName n1) (FieldName n2)
    )
    where
      n0 = spFieldLabelModifier opts "u_par0"
      n1 = spFieldLabelModifier opts "u_par1"
      n2 = spFieldLabelModifier opts "u_par2"

-- Param4 ----------------------------------------------------------------------

instance (GL.Uniform t, GL.Uniform u, GL.Uniform v, GL.Uniform w) => ShaderParams (Params4 t u v w Values) where
  shaderParams opts program = do
    loc0 <- GL.uniformLocation program n0
    loc1 <- GL.uniformLocation program n1
    loc2 <- GL.uniformLocation program n2
    loc3 <- GL.uniformLocation program n3
    
    when (loc0 < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n0 <> " not found"
    when (loc1 < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n1 <> " not found"
    when (loc2 < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n2 <> " not found"
    when (loc3 < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n3 <> " not found"

    pure $ \(Params4 t u v w) -> do
      GL.uniform loc0 $= t
      GL.uniform loc1 $= u
      GL.uniform loc2 $= v
      GL.uniform loc3 $= w
    where
      n0 = spFieldLabelModifier opts "u_par0"
      n1 = spFieldLabelModifier opts "u_par1"
      n2 = spFieldLabelModifier opts "u_par2"
      n3 = spFieldLabelModifier opts "u_par3"

instance (GLSLType t, GLSLType u, GLSLType v, GLSLType w) => NamedShaderParams (Params4 t u v w Fields) where
  namedShaderParams opts =
    ( [ (glslType $ Proxy @t, T.pack n0)
      , (glslType $ Proxy @u, T.pack n1)
      , (glslType $ Proxy @v, T.pack n2)
      , (glslType $ Proxy @w, T.pack n3)
      ]
    , Params4 (FieldName n0) (FieldName n1) (FieldName n2) (FieldName n3)
    )
    where
      n0 = spFieldLabelModifier opts "u_par0"
      n1 = spFieldLabelModifier opts "u_par1"
      n2 = spFieldLabelModifier opts "u_par2"
      n3 = spFieldLabelModifier opts "u_par3"
