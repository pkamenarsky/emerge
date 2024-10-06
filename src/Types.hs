{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE DeriveGeneric #-}

module Types where

import Control.Monad (when)

import qualified Data.Map.Strict as M
import Data.Proxy
import Data.StateVar
import Data.Text (Text)
import Data.Type.Bool
import qualified Data.Text as T

import qualified Graphics.Rendering.OpenGL as GL

import GHC.Generics (Generic, Rep, V1, U1 (U1), (:*:) ((:*:)), C, D, K1 (K1), M1 (M1), S, Meta (MetaSel), from, to)
import GHC.OverloadedLabels
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

--------------------------------------------------------------------------------

infixr 5 :.
data HList as where
  Nil :: HList '[]
  (:.) :: a -> HList as -> HList (a ': as)

data Param (s :: Symbol) t = Param t

type family IsEq (eq :: Ordering) :: Bool where
  IsEq 'EQ = 'True
  IsEq _ = 'False

type family ParamsEq p q :: Bool where
  ParamsEq (Param s t) (Param s' t) = IsEq (CmpSymbol s s')
  ParamsEq _ _ = 'False

type family Elem (x :: k) (xs :: [k]) :: Bool where
    Elem x '[]       = 'False
    Elem x (y ': xs) = If (ParamsEq x y) 'True (Elem x xs)

type family ElemSym (x :: Symbol) (xs :: [k]) :: Bool where
    ElemSym x '[]                = 'False
    ElemSym s (Param s' t ': xs) = If (IsEq (CmpSymbol s s')) 'True (ElemSym s xs)

type family Remove (x :: k) (xs :: [k]) :: [k] where
    Remove x '[]       = '[]
    Remove x (y ': xs) = If (ParamsEq x y) xs (y ': Remove x xs)

type family EqualSet (xs :: [k]) (ys :: [k]) :: Bool where
    EqualSet '[] '[] = 'True
    EqualSet (x ': xs) ys = Elem x ys && EqualSet xs (Remove x ys)
    EqualSet _ _ = 'False

type family SubSet (xs :: [k]) (ys :: [k]) :: Bool where
    SubSet '[] ys = 'True
    SubSet (x ': xs) ys = Elem x ys && SubSet xs (Remove x ys)
    SubSet _ _ = 'False

--------------------------------------------------------------------------------

newtype O a = O a

class FromTuples tuples hlist | tuples -> hlist where
  fromTuples :: tuples -> hlist

instance FromTuples (O a) (HList '[a]) where fromTuples (O a) = a :. Nil
instance FromTuples (a, b) (HList '[a, b]) where fromTuples (a, b) = a :. b :. Nil
instance FromTuples (a, b, c) (HList '[a, b, c]) where fromTuples (a, b, c) = a :. b :. c :. Nil
instance FromTuples (a, b, c, d) (HList '[a, b, c, d]) where fromTuples (a, b, c, d) = a :. b :. c :. d :. Nil
instance FromTuples (a, b, c, d, e) (HList '[a, b, c, d, e]) where fromTuples (a, b, c, d, e) = a :. b :. c :. d :. e :. Nil
instance FromTuples (a, b, c, d, e, f) (HList '[a, b, c, d, e, f]) where fromTuples (a, b, c, d, e, f) = a :. b :. c :. d :. e :. f :. Nil
instance FromTuples (a, b, c, d, e, f, g) (HList '[a, b, c, d, e, f, g]) where fromTuples (a, b, c, d, e, f, g) = a :. b :. c :. d :. e :. f :. g :. Nil

--------------------------------------------------------------------------------

class Params ps where
  params :: HList ps -> ([(Text, Text)], GL.Program -> IO [(String, GL.UniformLocation)])
  setUniform :: HList ps -> M.Map String GL.UniformLocation -> IO ()

instance Params '[] where
  params Nil = ([], mempty)
  setUniform _ _ = pure ()

instance (KnownSymbol s, GLSLType t, GL.Uniform t, Params ps) => Params (Param s t ': ps) where
  params (Param t :. ps) = ((T.pack $ symbolVal @s Proxy, glslType @t Proxy):fields, uniforms)
    where
      (fields, getUniforms') = params ps

      uniforms program = do
        loc <- GL.uniformLocation program (symbolVal @s Proxy)
        when (loc < GL.UniformLocation 0) $ error $ "params: uniform " <> symbolVal @s Proxy <> " not found"
        GL.uniform loc $= t

        uniforms' <- getUniforms' program

        pure ((symbolVal @s Proxy, loc):uniforms')
  setUniform (Param t :. ps) uniforms
    | Just loc <- M.lookup (symbolVal @s Proxy) uniforms = GL.uniform loc $= t
    | otherwise = setUniform ps uniforms

instance {-# OVERLAPPING #-} (KnownSymbol s, KnownNat n, Params ps) => Params (Param s (Texture n) ': ps) where
  params (Param (Texture t) :. ps) = ((T.pack $ symbolVal @s Proxy, "sampler2D"):fields, uniforms)
    where
      (fields, getUniforms') = params ps

      uniforms program = do
        loc <- GL.uniformLocation program (symbolVal @s Proxy)
        when (loc < GL.UniformLocation 0) $ error $ "params: uniform " <> symbolVal @s Proxy <> " not found"

        GL.activeTexture $= GL.TextureUnit (fromInteger $ natVal $ Proxy @n)
        GL.textureBinding GL.Texture2D $= Just t
        GL.uniform loc $= GL.TextureUnit (fromInteger $ natVal $ Proxy @n)

        uniforms' <- getUniforms' program

        pure ((symbolVal @s Proxy, loc):uniforms')
  setUniform (Param (Texture t) :. ps) uniforms
    | Just loc <- M.lookup (symbolVal @s Proxy) uniforms = do
        GL.activeTexture $= GL.TextureUnit (fromInteger $ natVal $ Proxy @n)
        GL.textureBinding GL.Texture2D $= Just t
        GL.uniform loc $= GL.TextureUnit (fromInteger $ natVal $ Proxy @n)
    | otherwise = setUniform ps uniforms

--------------------------------------------------------------------------------

data Name (s :: Symbol) = Name

instance (s ~ t) => IsLabel s (Name t) where
  fromLabel = Name

--------------------------------------------------------------------------------

vec2 :: Float -> Float -> GL.Vector2 Float
vec2 = GL.Vector2

vec3 :: Float -> Float -> Float -> GL.Vector3 Float
vec3 = GL.Vector3

float :: Float -> Float
float = id

int :: GL.GLint -> GL.GLint
int = id

tex :: GL.TextureObject -> Texture n
tex = Texture

data Set params
  = Set { set :: forall subtuples subparams. FromTuples subtuples (HList subparams) => Params subparams => SubSet subparams params ~ 'True => subtuples -> IO () }

shaderParams' :: FromTuples tuples (HList params) => Params params => tuples -> ([(Text, Text)], GL.Program -> IO (Set params))
shaderParams' tuples =
  ( fields
  , \program -> do
      uniforms <- M.fromList <$> initUniforms program
      pure $ Set $ \subtuples -> setUniform (fromTuples subtuples) uniforms
  )
  where
    (fields, initUniforms) = params (fromTuples tuples)

param, (=:) :: Name s -> t -> Param s t
param _ = Param
(=:) = param

field :: forall s params. ElemSym s params ~ 'True => KnownSymbol s => HList params -> Name s -> String
field _ _ = symbolVal @s Proxy

blaaaa = do
  t2 <- initt2 undefined
  set t2
    ( #lalal =: vec2 5 6
    , #was   =: vec3 5 6 7
    , #max_iterations =: int 6
    )
  where
    (_, initt2) = shaderParams'
      ( #lalal =: vec2 5 6
      , #was   =: vec3 5 6 7
      , #max_iterations =: int 6
      , #normal_map =: tex @0 undefined
      )

