{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE DeriveGeneric #-}

module Types where

import Control.Monad (when)

import Data.Proxy
import Data.StateVar
import Data.Word (Word32)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Graphics.Rendering.OpenGL as GL

import GHC.Int
import GHC.Generics (Generic, Rep, V1, U1 (U1), (:*:) ((:*:)), C, D, K1 (K1), M1 (M1), S, Meta (MetaSel), from, to)
import GHC.OverloadedLabels
import GHC.Records
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

instance GLSLType (GL.Vector2 Float) where
  glslType _ = "vec2"

instance GLSLType (GL.Vector3 Float) where
  glslType _ = "vec3"

--------------------------------------------------------------------------------

data Fields
data Values

type family P f v where
  P Fields t = Text
  P Values t = t

--------------------------------------------------------------------------------

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
  namedShaderParam :: ShaderParamDeriveOpts -> ([(Text, Text)], a Fields)
  default namedShaderParam
    :: Generic (a Fields)
    => GNamedShaderParam (Rep (a Values)) (Rep (a Fields))
    => ShaderParamDeriveOpts
    -> ([(Text, Text)], a Fields)
  namedShaderParam opts = fmap to $ gNamedShaderParam (Proxy :: Proxy (Rep (a Values) x)) opts

--------------------------------------------------------------------------------

data (s :: Symbol) ::: t = Param Text t

type family FromTuples (tuples :: *) :: [*] where
  FromTuples (a ::: t) = '[a ::: t]
  FromTuples (a ::: t, b ::: u) = '[a ::: t, b ::: u]
  FromTuples (a ::: t, b ::: u, c ::: v) = '[a ::: t, b ::: u, c ::: v]
  FromTuples (a ::: t, b ::: u, c ::: v, d ::: w) = '[a ::: t, b ::: u, c ::: v, d ::: w]
  FromTuples (a ::: t, b ::: u, c ::: v, d ::: w, e ::: x) = '[a ::: t, b ::: u, c ::: v, d ::: w, e ::: x]
  FromTuples (a ::: t, b ::: u, c ::: v, d ::: w, e ::: x, f ::: y) = '[a ::: t, b ::: u, c ::: v, d ::: w, e ::: x, f ::: y]
  FromTuples (a ::: t, b ::: u, c ::: v, d ::: w, e ::: x, f ::: y, g ::: z) = '[a ::: t, b ::: u, c ::: v, d ::: w, e ::: x, f ::: y, g ::: z]

type family ToTuples (params :: [*]) where
  ToTuples '[a ::: t] = a ::: t
  ToTuples '[a ::: t, b ::: u] = (a ::: t, b ::: u)
  ToTuples '[a ::: t, b ::: u, c ::: v] = (a ::: t, b ::: u, c ::: v)
  ToTuples '[a ::: t, b ::: u, c ::: v, d ::: w] = (a ::: t, b ::: u, c ::: v, d ::: w)
  ToTuples '[a ::: t, b ::: u, c ::: v, d ::: w, e ::: x] = (a ::: t, b ::: u, c ::: v, d ::: w, e ::: x)
  ToTuples '[a ::: t, b ::: u, c ::: v, d ::: w, e ::: x, f ::: y] = (a ::: t, b ::: u, c ::: v, d ::: w, e ::: x, f ::: y)
  ToTuples '[a ::: t, b ::: u, c ::: v, d ::: w, e ::: x, f ::: y, g ::: z] = (a ::: t, b ::: u, c ::: v, d ::: w, e ::: x, f ::: y, g ::: z)

type family Lookup (s :: Symbol) (params :: [*]) :: Maybe * where
  Lookup s '[] = TypeError ('Text "Lookup")
  Lookup s (s ::: t : ps) = 'Just t
  Lookup s (p:ps) = Lookup s ps

data Record (params :: [*]) = Record (ToTuples params)

instance Lookup s fields ~ Just t => HasField (Record fields) (Proxy s) t

record :: list ~ FromTuples tuples => ToTuples list ~ tuples => tuples -> Record list
record tuples = Record tuples

param :: KnownSymbol s => HasField (Record fields) (Proxy s) t => Record fields -> s ::: t
param = undefined

field :: forall s t. KnownSymbol s => t -> s ::: t
field t = Param (T.pack $ symbolVal $ Proxy @s) t

shader :: Record params -> (Record params -> IO ()) -> IO ()
shader = undefined

lalala = shader (record (field @"name" 5.5, field @"age" 6)) bla
  where
    bla params = undefined
      where
        a = param @"name" params
        b = param @"age" params

-- infixr 5 :.
-- data List as where
--   Nil :: List '[]
--   (:.) :: a -> List as -> List (a ': as)

-- type family (++) (xs :: [*]) (ys :: [*]) :: [*] where
--   '[]       ++ ys = ys
--   (x ': xs) ++ ys = x ': (xs ++ ys)
-- 
-- data Label (s :: Symbol) = Label Text
-- 
-- instance KnownSymbol s => IsLabel s (Label s) where
--   fromLabel = Label (T.pack $ symbolVal $ Proxy @s)
-- 
-- param :: forall s t. KnownSymbol s => t -> (s ::: t)
-- param t = Param (T.pack $ symbolVal $ Proxy @s) t
-- 
-- class RecordParams params where
--   field :: params -> Label s -> Text
--   params :: params -> [(Text, Text)]
-- 
-- merge :: Record list1 -> Record list2 -> Record (list1 ++ list2)
-- merge (Record tuples) = undefined
-- 
-- bla = record (param @"name" 4.5, param @"age" 7)
