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
    loc <- GL.uniformLocation program n
    
    when (loc < GL.UniformLocation 0) $ error $ "gShaderParam: uniform " <> n <> " not found"

    pure $ \(M1 (K1 a)) -> GL.uniform loc $= a
    where
      n = spFieldLabelModifier opts $ symbolVal $ Proxy @name

instance {-# OVERLAPPING #-} (KnownSymbol name, KnownNat n) => GShaderParam (M1 S ('MetaSel ('Just name) u s t) (K1 i (Texture n))) where
  gShaderParam opts program = do
    loc <- GL.uniformLocation program n
    
    when (loc < GL.UniformLocation 0) $ error $ "gShaderParam: uniform " <> n <> " not found"

    pure $ \(M1 (K1 (Texture tex))) -> do
      GL.activeTexture $= GL.TextureUnit (fromInteger $ natVal $ Proxy @n)
      GL.textureBinding GL.Texture2D $= Just tex
      GL.uniform loc $= GL.TextureUnit (fromInteger $ natVal $ Proxy @n)
    where
      n = spFieldLabelModifier opts $ symbolVal $ Proxy @name

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

data Param2 t u m = Param2 (P m t) (P m u)
data Param3 t u v m = Param3 (P m t) (P m u) (P m v)
data Param4 t u v w m = Param4 (P m t) (P m u) (P m v) (P m w)

--------------------------------------------------------------------------------

class GNamedShaderParam f where                                                           
  gNamedShaderParam :: ShaderParamDeriveOpts -> ([(Text, Text)], f a)

instance GNamedShaderParam U1 where gNamedShaderParam _ = ([], U1)

instance (GNamedShaderParam g, GNamedShaderParam f) => GNamedShaderParam (g :*: f) where
  gNamedShaderParam opts = (ap <> bp, a :*: b)
    where
      (ap, a) = gNamedShaderParam opts
      (bp, b) = gNamedShaderParam opts

instance GNamedShaderParam f => GNamedShaderParam (M1 C i f) where
  gNamedShaderParam opts = M1 <$> gNamedShaderParam opts

instance GNamedShaderParam f => GNamedShaderParam (M1 D i f) where
  gNamedShaderParam opts = M1 <$> gNamedShaderParam opts

instance (KnownSymbol name, GLSLType g) => GNamedShaderParam (M1 S ('MetaSel ('Just name) u s t) (K1 i (FieldName g))) where
  gNamedShaderParam opts = ([(glslType $ Proxy @g, T.pack fieldName)], M1 (K1 $ FieldName fieldName))
    where
      fieldName = spFieldLabelModifier opts $ symbolVal $ Proxy @name

class NamedShaderParam a where
  namedShaderParam :: ShaderParamDeriveOpts -> ([(Text, Text)], a)
  default namedShaderParam :: Generic a => GNamedShaderParam (Rep a) => ShaderParamDeriveOpts -> ([(Text, Text)], a)
  namedShaderParam = fmap to . gNamedShaderParam

-- Only ------------------------------------------------------------------------

instance GL.Uniform t => ShaderParam (Only t Values) where
  shaderParam opts program = do
    loc <- GL.uniformLocation program n
    
    when (loc < GL.UniformLocation 0) $ error $ "gShaderParam: uniform " <> n <> " not found"

    pure $ \(Only t) -> GL.uniform loc $= t
    where
      n = spFieldLabelModifier opts "u_par"

instance GLSLType t => NamedShaderParam (Only t Fields) where
  namedShaderParam opts = ([(T.pack n, glslType $ Proxy @t)], Only $ FieldName n)
    where
      n = spFieldLabelModifier opts "u_par"

-- Param2 ----------------------------------------------------------------------

instance (GL.Uniform t, GL.Uniform u) => ShaderParam (Param2 t u Values) where
  shaderParam opts program = do
    loc0 <- GL.uniformLocation program n0
    loc1 <- GL.uniformLocation program n1
    
    when (loc0 < GL.UniformLocation 0) $ error $ "gShaderParam: uniform " <> n0 <> " not found"
    when (loc1 < GL.UniformLocation 0) $ error $ "gShaderParam: uniform " <> n1 <> " not found"

    pure $ \(Param2 t u) -> do
      GL.uniform loc0 $= t
      GL.uniform loc1 $= u
    where
      n0 = spFieldLabelModifier opts "u_par0"
      n1 = spFieldLabelModifier opts "u_par1"

instance (GLSLType t, GLSLType u) => NamedShaderParam (Param2 t u Fields) where
  namedShaderParam opts =
    ( [ (T.pack n0, glslType $ Proxy @t)
      , (T.pack n1, glslType $ Proxy @u)
      ]
    , Param2 (FieldName n0) (FieldName n1)
    )
    where
      n0 = spFieldLabelModifier opts "u_par0"
      n1 = spFieldLabelModifier opts "u_par1"

-- Param3 ----------------------------------------------------------------------

instance (GL.Uniform t, GL.Uniform u, GL.Uniform v) => ShaderParam (Param3 t u v Values) where
  shaderParam opts program = do
    loc0 <- GL.uniformLocation program n0
    loc1 <- GL.uniformLocation program n1
    loc2 <- GL.uniformLocation program n2
    
    when (loc0 < GL.UniformLocation 0) $ error $ "gShaderParam: uniform " <> n0 <> " not found"
    when (loc1 < GL.UniformLocation 0) $ error $ "gShaderParam: uniform " <> n1 <> " not found"
    when (loc2 < GL.UniformLocation 0) $ error $ "gShaderParam: uniform " <> n2 <> " not found"

    pure $ \(Param3 t u v) -> do
      GL.uniform loc0 $= t
      GL.uniform loc1 $= u
      GL.uniform loc2 $= v
    where
      n0 = spFieldLabelModifier opts "u_par0"
      n1 = spFieldLabelModifier opts "u_par1"
      n2 = spFieldLabelModifier opts "u_par2"

instance (GLSLType t, GLSLType u, GLSLType v) => NamedShaderParam (Param3 t u v Fields) where
  namedShaderParam opts =
    ( [ (T.pack n0, glslType $ Proxy @t)
      , (T.pack n1, glslType $ Proxy @u)
      , (T.pack n2, glslType $ Proxy @v)
      ]
    , Param3 (FieldName n0) (FieldName n1) (FieldName n2)
    )
    where
      n0 = spFieldLabelModifier opts "u_par0"
      n1 = spFieldLabelModifier opts "u_par1"
      n2 = spFieldLabelModifier opts "u_par2"

-- Param4 ----------------------------------------------------------------------

instance (GL.Uniform t, GL.Uniform u, GL.Uniform v, GL.Uniform w) => ShaderParam (Param4 t u v w Values) where
  shaderParam opts program = do
    loc0 <- GL.uniformLocation program n0
    loc1 <- GL.uniformLocation program n1
    loc2 <- GL.uniformLocation program n2
    loc3 <- GL.uniformLocation program n3
    
    when (loc0 < GL.UniformLocation 0) $ error $ "gShaderParam: uniform " <> n0 <> " not found"
    when (loc1 < GL.UniformLocation 0) $ error $ "gShaderParam: uniform " <> n1 <> " not found"
    when (loc2 < GL.UniformLocation 0) $ error $ "gShaderParam: uniform " <> n2 <> " not found"
    when (loc3 < GL.UniformLocation 0) $ error $ "gShaderParam: uniform " <> n3 <> " not found"

    pure $ \(Param4 t u v w) -> do
      GL.uniform loc0 $= t
      GL.uniform loc1 $= u
      GL.uniform loc2 $= v
      GL.uniform loc3 $= w
    where
      n0 = spFieldLabelModifier opts "u_par0"
      n1 = spFieldLabelModifier opts "u_par1"
      n2 = spFieldLabelModifier opts "u_par2"
      n3 = spFieldLabelModifier opts "u_par3"

instance (GLSLType t, GLSLType u, GLSLType v, GLSLType w) => NamedShaderParam (Param4 t u v w Fields) where
  namedShaderParam opts =
    ( [ (T.pack n0, glslType $ Proxy @t)
      , (T.pack n1, glslType $ Proxy @u)
      , (T.pack n2, glslType $ Proxy @v)
      , (T.pack n3, glslType $ Proxy @w)
      ]
    , Param4 (FieldName n0) (FieldName n1) (FieldName n2) (FieldName n3)
    )
    where
      n0 = spFieldLabelModifier opts "u_par0"
      n1 = spFieldLabelModifier opts "u_par1"
      n2 = spFieldLabelModifier opts "u_par2"
      n3 = spFieldLabelModifier opts "u_par3"
