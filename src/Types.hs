{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE DeriveGeneric #-}

module Types where

import Control.Monad.IO.Class
import Control.Monad (when)

import qualified Data.Map.Strict as M
import Data.Proxy
import Data.StateVar hiding (get)
import Data.Text (Text)
import Data.Type.Bool
import qualified Data.Text as T
import Data.Word
import Data.Void

import qualified Graphics.Rendering.OpenGL as GL

import GHC.Generics (Generic, Rep, V1, U1 (U1), (:*:) ((:*:)), C, D, K1 (K1), M1 (M1), S, Meta (MetaSel), from, to)
import GHC.OverloadedLabels
import GHC.TypeLits

--------------------------------------------------------------------------------

class Default a where
  def :: a

x :: Default a => a
x = def

newtype Texture (n :: Nat) = Texture (Maybe GL.TextureObject)

--------------------------------------------------------------------------------

class GLSLType t where glslType :: Proxy t -> Text

instance GLSLType GL.GLint where glslType _ = "int"
-- instance GLSLType GL.GLuint where glslType _ = "uint"
instance GLSLType Float where glslType _ = "float"
instance GLSLType Double where glslType _ = "double"
instance GLSLType (GL.Vector2 Float) where glslType _ = "vec2"
instance GLSLType (GL.Vector3 Float) where glslType _ = "vec3"
instance GLSLType (GL.Vector4 Float) where glslType _ = "vec4"
instance GLSLType (GL.Color4 Float) where glslType _ = "vec4"

--------------------------------------------------------------------------------

data ShaderParamDeriveOpts = ShaderParamDeriveOpts
  { spFieldLabelModifier :: String -> String
  }

instance Default ShaderParamDeriveOpts where
  def = ShaderParamDeriveOpts id

class GShaderParams f where                                                           
  gShaderParams :: ShaderParamDeriveOpts -> ([(Text, Text)], GL.Program -> IO (f a -> IO ()))

instance GShaderParams V1 where gShaderParams _ = ([], \_ -> pure $ \_ -> pure ())
instance GShaderParams U1 where gShaderParams _ = ([], \_ -> pure $ \_ -> pure ())

instance GShaderParams a => GShaderParams (M1 C i a) where
  gShaderParams opts =
    ( fields
    , \program -> do
         set <- init program
         pure $ \(M1 a) -> set a
    )
    where
      (fields, init) = gShaderParams opts

instance GShaderParams a => GShaderParams (M1 D i a) where
  gShaderParams opts =
    ( fields
    , \program -> do
         set <- init program
         pure $ \(M1 a) -> set a
    )
    where
      (fields, init) = gShaderParams opts

instance (GShaderParams a, GShaderParams b) => GShaderParams (a :*: b) where
  gShaderParams opts =
    ( fieldsA <> fieldsB
    , \program -> do
         setA <- initA program
         setB <- initB program

         pure $ \(a :*: b) -> setA a >> setB b
    )
    where
      (fieldsA, initA) = gShaderParams opts
      (fieldsB, initB) = gShaderParams opts
      

instance {-# OVERLAPPABLE #-} (KnownSymbol name, GLSLType a, GL.Uniform a) => GShaderParams (M1 S ('MetaSel ('Just name) u s t) (K1 i (Signal a))) where
  gShaderParams opts =
    ( [(glslType $ Proxy @a, T.pack $ symbolVal $ Proxy @name)]
    , \program -> do
         loc <- GL.uniformLocation program n
         when (loc < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n <> " not found"

         pure $ \(M1 (K1 a)) -> signalValue a >>= (GL.uniform loc $=)
    )
    where
      n = spFieldLabelModifier opts $ symbolVal $ Proxy @name

instance {-# OVERLAPPING #-} (KnownSymbol name, KnownNat n) => GShaderParams (M1 S ('MetaSel ('Just name) u s t) (K1 i (Signal (Texture n)))) where
  gShaderParams opts =
    ( [("sampler2D", T.pack $ symbolVal $ Proxy @name)]
    , \program -> do
         loc <- GL.uniformLocation program n
         when (loc < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n <> " not found"

         pure $ \(M1 (K1 a)) -> do
           GL.activeTexture $= GL.TextureUnit (fromInteger $ natVal $ Proxy @n)
           Texture tex <- signalValue a
           GL.textureBinding GL.Texture2D $= tex
           GL.uniform loc $= GL.TextureUnit (fromInteger $ natVal $ Proxy @n)
    )
    where
      n = spFieldLabelModifier opts $ symbolVal $ Proxy @name

class ShaderParams a where
  shaderParams :: ShaderParamDeriveOpts -> (ParamFields a, GL.Program -> IO (a -> IO ()))

instance (Generic a, GShaderParams (Rep a)) => ShaderParams a where
  shaderParams opts =
    ( ParamFields opts fields
    , \program -> do
         set <- init program
         pure $ \a -> set (from a)
    )
    where
      (fields, init) = gShaderParams opts

--------------------------------------------------------------------------------

class GGetField a s t | a s -> t where
  gGetField :: a x -> Name s -> t

instance GGetField V1 s Void where gGetField _ _ = undefined
instance GGetField U1 s () where gGetField _ _ = ()
instance GGetField a s t => GGetField (M1 D i a) s t where gGetField (M1 a) s = gGetField a s
instance GGetField a s t => GGetField (M1 C i a) s t where gGetField (M1 a) s = gGetField a s

instance GGetField (M1 S ('MetaSel ('Just s) i a b) (K1 r t) :*: g) s t where
  gGetField (M1 (K1 t) :*: _) _ = t

instance {-# OVERLAPPABLE #-} GGetField g s t => GGetField (f :*: g) s t where
  gGetField (_ :*: g) s = gGetField g s

instance GGetField (M1 S ('MetaSel ('Just s) i a b) (K1 r t)) s t where
  gGetField (M1 (K1 t)) _ = t

class GetField a s t | a s -> t where
  getField :: a -> Name s -> t

instance (Generic a, GGetField (Rep a) s t) => GetField a s t where
  getField a s = gGetField (from a) s

--------------------------------------------------------------------------------

uniform :: forall a s t. (KnownSymbol s, GetField a s t) => ParamFields a -> Name s -> String
uniform (ParamFields opts _) _ = spFieldLabelModifier opts $ symbolVal $ Proxy @s

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

-- instance GLSLType t => NamedShaderParams (Only t Fields) where
--   namedShaderParams opts = ([(glslType $ Proxy @t, T.pack n)], Only $ FieldName n)
--     where
--       n = spFieldLabelModifier opts "u_par"

-- Param2 ----------------------------------------------------------------------

-- instance (GL.Uniform t, GL.Uniform u) => ShaderParams (Params2 t u Values) where
--   shaderParams opts program = do
--     loc0 <- GL.uniformLocation program n0
--     loc1 <- GL.uniformLocation program n1
--     
--     when (loc0 < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n0 <> " not found"
--     when (loc1 < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n1 <> " not found"
-- 
--     pure $ \(Params2 t u) -> do
--       GL.uniform loc0 $= t
--       GL.uniform loc1 $= u
--     where
--       n0 = spFieldLabelModifier opts "u_par0"
--       n1 = spFieldLabelModifier opts "u_par1"
-- 
-- instance (GLSLType t, GLSLType u) => NamedShaderParams (Params2 t u Fields) where
--   namedShaderParams opts =
--     ( [ (glslType $ Proxy @t, T.pack n0)
--       , (glslType $ Proxy @u, T.pack n1)
--       ]
--     , Params2 (FieldName n0) (FieldName n1)
--     )
--     where
--       n0 = spFieldLabelModifier opts "u_par0"
--       n1 = spFieldLabelModifier opts "u_par1"
-- 
-- -- Param3 ----------------------------------------------------------------------
-- 
-- instance (GL.Uniform t, GL.Uniform u, GL.Uniform v) => ShaderParams (Params3 t u v Values) where
--   shaderParams opts program = do
--     loc0 <- GL.uniformLocation program n0
--     loc1 <- GL.uniformLocation program n1
--     loc2 <- GL.uniformLocation program n2
--     
--     when (loc0 < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n0 <> " not found"
--     when (loc1 < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n1 <> " not found"
--     when (loc2 < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n2 <> " not found"
-- 
--     pure $ \(Params3 t u v) -> do
--       GL.uniform loc0 $= t
--       GL.uniform loc1 $= u
--       GL.uniform loc2 $= v
--     where
--       n0 = spFieldLabelModifier opts "u_par0"
--       n1 = spFieldLabelModifier opts "u_par1"
--       n2 = spFieldLabelModifier opts "u_par2"
-- 
-- instance (GLSLType t, GLSLType u, GLSLType v) => NamedShaderParams (Params3 t u v Fields) where
--   namedShaderParams opts =
--     ( [ (glslType $ Proxy @t, T.pack n0)
--       , (glslType $ Proxy @u, T.pack n1)
--       , (glslType $ Proxy @v, T.pack n2)
--       ]
--     , Params3 (FieldName n0) (FieldName n1) (FieldName n2)
--     )
--     where
--       n0 = spFieldLabelModifier opts "u_par0"
--       n1 = spFieldLabelModifier opts "u_par1"
--       n2 = spFieldLabelModifier opts "u_par2"
-- 
-- -- Param4 ----------------------------------------------------------------------
-- 
-- instance (GL.Uniform t, GL.Uniform u, GL.Uniform v, GL.Uniform w) => ShaderParams (Params4 t u v w Values) where
--   shaderParams opts program = do
--     loc0 <- GL.uniformLocation program n0
--     loc1 <- GL.uniformLocation program n1
--     loc2 <- GL.uniformLocation program n2
--     loc3 <- GL.uniformLocation program n3
--     
--     when (loc0 < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n0 <> " not found"
--     when (loc1 < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n1 <> " not found"
--     when (loc2 < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n2 <> " not found"
--     when (loc3 < GL.UniformLocation 0) $ error $ "gShaderParams: uniform " <> n3 <> " not found"
-- 
--     pure $ \(Params4 t u v w) -> do
--       GL.uniform loc0 $= t
--       GL.uniform loc1 $= u
--       GL.uniform loc2 $= v
--       GL.uniform loc3 $= w
--     where
--       n0 = spFieldLabelModifier opts "u_par0"
--       n1 = spFieldLabelModifier opts "u_par1"
--       n2 = spFieldLabelModifier opts "u_par2"
--       n3 = spFieldLabelModifier opts "u_par3"
-- 
-- instance (GLSLType t, GLSLType u, GLSLType v, GLSLType w) => NamedShaderParams (Params4 t u v w Fields) where
--   namedShaderParams opts =
--     ( [ (glslType $ Proxy @t, T.pack n0)
--       , (glslType $ Proxy @u, T.pack n1)
--       , (glslType $ Proxy @v, T.pack n2)
--       , (glslType $ Proxy @w, T.pack n3)
--       ]
--     , Params4 (FieldName n0) (FieldName n1) (FieldName n2) (FieldName n3)
--     )
--     where
--       n0 = spFieldLabelModifier opts "u_par0"
--       n1 = spFieldLabelModifier opts "u_par1"
--       n2 = spFieldLabelModifier opts "u_par2"
--       n3 = spFieldLabelModifier opts "u_par3"

--------------------------------------------------------------------------------

newtype Signal a = Signal (IO a)
  deriving (Functor, Applicative, Monad)

signalValue :: MonadIO m => Signal a -> m a
signalValue (Signal v) = liftIO v

signalValueIO :: Signal a -> IO a
signalValueIO (Signal v) = v

--------------------------------------------------------------------------------

infixr 5 :.
data HList as where
  Nil :: HList '[]
  (:.) :: a -> HList as -> HList (a ': as)

newtype Param (s :: Symbol) t = Param t
newtype SigParam (s :: Symbol) t = SigParam (Signal t)

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

class FromTuples tuples hlist | tuples -> hlist where
  fromTuples :: tuples -> hlist

instance FromTuples () (HList '[]) where fromTuples () = Nil
instance FromTuples (Param s t) (HList '[Param s t]) where fromTuples p = p :. Nil
instance FromTuples (a, b) (HList '[a, b]) where fromTuples (a, b) = a :. b :. Nil
instance FromTuples (a, b, c) (HList '[a, b, c]) where fromTuples (a, b, c) = a :. b :. c :. Nil
instance FromTuples (a, b, c, d) (HList '[a, b, c, d]) where fromTuples (a, b, c, d) = a :. b :. c :. d :. Nil
instance FromTuples (a, b, c, d, e) (HList '[a, b, c, d, e]) where fromTuples (a, b, c, d, e) = a :. b :. c :. d :. e :. Nil
instance FromTuples (a, b, c, d, e, f) (HList '[a, b, c, d, e, f]) where fromTuples (a, b, c, d, e, f) = a :. b :. c :. d :. e :. f :. Nil
instance FromTuples (a, b, c, d, e, f, g) (HList '[a, b, c, d, e, f, g]) where fromTuples (a, b, c, d, e, f, g) = a :. b :. c :. d :. e :. f :. g :. Nil

--------------------------------------------------------------------------------

class Params ps where
  initParams :: HList ps -> ([(Text, Text)], GL.Program -> IO [(String, GL.UniformLocation)])
  setUniform :: HList ps -> M.Map String GL.UniformLocation -> IO ()

instance Params '[] where
  initParams Nil = ([], mempty)
  setUniform _ _ = pure ()

instance (KnownSymbol s, GLSLType t, GL.Uniform t, Params ps) => Params (SigParam s t ': ps) where
  initParams (SigParam t :. ps) = ((glslType @t Proxy, T.pack $ symbolVal @s Proxy):fields, uniforms)
    where
      (fields, getUniforms') = initParams ps

      uniforms program = do
        loc <- GL.uniformLocation program (symbolVal @s Proxy)
        when (loc < GL.UniformLocation 0) $ error $ "params: uniform " <> symbolVal @s Proxy <> " not found"
        v <- signalValue t
        GL.uniform loc $= v

        uniforms' <- getUniforms' program

        pure ((symbolVal @s Proxy, loc):uniforms')

  setUniform (SigParam t :. ps) uniforms
    | Just loc <- M.lookup (symbolVal @s Proxy) uniforms = do
        v <- signalValue t
        GL.uniform loc $= v
        setUniform ps uniforms

    | otherwise = setUniform ps uniforms

instance (KnownSymbol s, GLSLType t, GL.Uniform t, Params ps) => Params (Param s t ': ps) where
  initParams (Param t :. ps) = ((glslType @t Proxy, T.pack $ symbolVal @s Proxy):fields, uniforms)
    where
      (fields, getUniforms') = initParams ps

      uniforms program = do
        loc <- GL.uniformLocation program (symbolVal @s Proxy)
        when (loc < GL.UniformLocation 0) $ error $ "params: uniform " <> symbolVal @s Proxy <> " not found"
        GL.uniform loc $= t

        uniforms' <- getUniforms' program

        pure ((symbolVal @s Proxy, loc):uniforms')

  setUniform (Param t :. ps) uniforms
    | Just loc <- M.lookup (symbolVal @s Proxy) uniforms = do
        GL.uniform loc $= t
        setUniform ps uniforms

    | otherwise = setUniform ps uniforms

instance {-# OVERLAPPING #-} (KnownSymbol s, KnownNat n, Params ps) => Params (Param s (Texture n) ': ps) where
  initParams (Param (Texture t) :. ps) = (("sampler2D", T.pack $ symbolVal @s Proxy):fields, uniforms)
    where
      (fields, getUniforms') = initParams ps

      uniforms program = do
        loc <- GL.uniformLocation program (symbolVal @s Proxy)
        when (loc < GL.UniformLocation 0) $ error $ "params: uniform " <> symbolVal @s Proxy <> " not found"

        GL.activeTexture $= GL.TextureUnit (fromInteger $ natVal $ Proxy @n)
        GL.textureBinding GL.Texture2D $= t
        GL.uniform loc $= GL.TextureUnit (fromInteger $ natVal $ Proxy @n)

        uniforms' <- getUniforms' program

        pure ((symbolVal @s Proxy, loc):uniforms')

  setUniform (Param (Texture t) :. ps) uniforms
    | Just loc <- M.lookup (symbolVal @s Proxy) uniforms = do
        GL.activeTexture $= GL.TextureUnit (fromInteger $ natVal $ Proxy @n)
        GL.textureBinding GL.Texture2D $= t
        GL.uniform loc $= GL.TextureUnit (fromInteger $ natVal $ Proxy @n)
        setUniform ps uniforms

    | otherwise = setUniform ps uniforms

--------------------------------------------------------------------------------

class SeqParams s v | s -> v where
  seqParams :: HList s -> IO (HList v)

instance SeqParams '[] '[] where
  seqParams _ = pure Nil

instance SeqParams ss vs => SeqParams (Param s (Signal t) ': ss) (Param s t ': vs) where
  seqParams (Param t :. ss) = do
    v <- signalValue t
    vs <- seqParams ss
    pure (Param v :. vs)

--------------------------------------------------------------------------------

data Name (s :: Symbol) = Name

instance (s ~ t) => IsLabel s (Name t) where
  fromLabel = Name

--------------------------------------------------------------------------------

type Vec2 = GL.Vector2 Float
type Vec3 = GL.Vector3 Float
type Vec4 = GL.Vector4 Float
type Color4 = GL.Color4 Float
type GLint = GL.GLint

vec2 :: Float -> Float -> Vec2
vec2 = GL.Vector2

vec3 :: Float -> Float -> Float -> Vec3
vec3 = GL.Vector3

vec4 :: Float -> Float -> Float -> Float -> Vec4
vec4 = GL.Vector4

color4 :: Float -> Float -> Float -> Float -> Color4
color4 = GL.Color4

rgba :: Word8 -> Word8 -> Word8 -> Word8 -> Color4
rgba r g b a = GL.Color4 (c r) (c g) (c b) (c a)
  where
    c x = fromIntegral x / 255.0

float :: Float -> Float
float = id

int :: GL.GLint -> GL.GLint
int = id

texture :: Maybe GL.TextureObject -> Texture n
texture = Texture

data Set params = Set { set :: forall subparams. Params subparams => SubSet subparams params ~ 'True => HList subparams -> IO () }

data ParamFields params = ParamFields ShaderParamDeriveOpts [(Text, Text)]

paramUniforms :: ParamFields params -> [(Text, Text)]
paramUniforms (ParamFields opts uniforms) =
  [ (ut, T.pack $ spFieldLabelModifier opts $ T.unpack un)
  | (ut, un) <- uniforms
  ]

formatParamUniforms :: ParamFields params -> Text
formatParamUniforms paramFields = T.intercalate "\n"
  [ "uniform " <> ut <> " " <> un <> ";"
  | (ut, un) <- paramUniforms paramFields
  ]

shaderParams' :: Params params => ShaderParamDeriveOpts -> HList params -> (ParamFields params, GL.Program -> IO (Set params))
shaderParams' opts params =
  ( ParamFields opts fields
  , \program -> do
      uniforms <- M.fromList <$> initUniforms program
      pure $ Set $ \subtuples -> setUniform subtuples uniforms
  )
  where
    (fields, initUniforms) = initParams params

shaderParams'' :: FromTuples tuples (HList params) => Params params => ShaderParamDeriveOpts -> tuples -> (ParamFields params, GL.Program -> IO (Set params))
shaderParams'' opts = shaderParams' opts . fromTuples

param, (=:) :: Name s -> t -> Param s t
param _ = Param

(=:) = param
infixr 0 =:

instance (KnownSymbol s, ElemSym s params ~ 'True) => IsLabel s (ParamFields params -> String) where
  fromLabel = \(ParamFields opts _) -> spFieldLabelModifier opts $ symbolVal @s Proxy

field :: forall s params. KnownSymbol s => ElemSym s params ~ 'True => ParamFields params -> Name s -> String
field (ParamFields opts _) _ = spFieldLabelModifier opts $ symbolVal @s Proxy

--------------------------------------------------------------------------------

-- class GetField params s t | params s -> t where
--   getField :: HList params -> Name s -> t
-- 
-- instance TypeError ('Text "No such field: " ':<>: 'Text s) => GetField '[] s Void where
--   getField _ _ = undefined
-- 
-- instance {-# OVERLAPPING #-} GetField (Param s t ': ps) s t where
--   getField (Param t :. _) _ = t
-- 
-- instance GetField ps s t => GetField (Param k u ': ps) s t where
--   getField (_ :. ps) s = getField ps s
-- 
-- params0 = (#abc =: float 5, #def =: float 6)
-- 
-- instance (FromTuples tuples (HList params), GetField params s t) => IsLabel s (tuples -> t) where
--   fromLabel = \tuples -> getField (fromTuples tuples) (Name @s)
-- 
-- get' :: FromTuples tuples (HList params) => GetField params s t => tuples -> Name s -> t
-- get' tuples s = getField (fromTuples tuples) s
-- 
-- f = getField (fromTuples params0) (Name @"def")
-- 
-- f' = #abc params0
