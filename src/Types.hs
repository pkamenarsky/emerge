{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Types where

import Control.Monad.IO.Class
import Control.Monad (when)

import Data.Proxy
import Data.StateVar hiding (get)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word

import Foreign.Ptr

import qualified Graphics.Rendering.OpenGL as GL

import GHC.Generics
import GHC.OverloadedLabels
import GHC.Records
import GHC.TypeLits

import Debug.Trace

--------------------------------------------------------------------------------

class Default a where
  def :: a

o :: Default a => a
o = def

  --------------------------------------------------------------------------------

data ShaderParamDeriveOpts = ShaderParamDeriveOpts
  { spFieldLabelModifier :: String -> String
  }

instance Default ShaderParamDeriveOpts where
  def = ShaderParamDeriveOpts id

class GShaderParams f where                                                           
  gShaderParams :: ShaderParamDeriveOpts -> f a -> ([(Text, Text)], GL.Program -> IO (IO ()))

instance GShaderParams V1 where gShaderParams _ _ = ([], \_ -> pure $ pure ())
instance GShaderParams U1 where gShaderParams _ _ = ([], \_ -> pure $ pure ())

instance GShaderParams a => GShaderParams (M1 C i a) where
  gShaderParams opts (M1 a) = gShaderParams opts a

instance GShaderParams a => GShaderParams (M1 D i a) where
  gShaderParams opts (M1 a) = gShaderParams opts a

instance (GShaderParams a, GShaderParams b) => GShaderParams (a :*: b) where
  gShaderParams opts (a :*: b) =
    ( fieldsA <> fieldsB
    , \program -> do
         setA <- initA program
         setB <- initB program

         pure $ setA >> setB
    )
    where
      (fieldsA, initA) = gShaderParams opts a
      (fieldsB, initB) = gShaderParams opts b

instance (KnownSymbol name, GLSLType a, GL.Uniform a) => GShaderParams (M1 S ('MetaSel ('Just name) u s t) (K1 i (Signal a))) where
  gShaderParams opts (M1 (K1 a)) =
    ( [(glslType $ Proxy @a, T.pack $ symbolVal $ Proxy @name)]
    , \program -> do
         loc <- GL.uniformLocation program n

         when (loc < GL.UniformLocation 0) $
           traceIO $ "WARNING: gShaderParams: uniform " <> n <> " not found"

         pure $ signalValue a >>= maybe (pure ()) (GL.uniform loc $=)
    )
    where
      n = spFieldLabelModifier opts $ symbolVal $ Proxy @name

--------------------------------------------------------------------------------

data ParamFields params = ParamFields ShaderParamDeriveOpts [(Text, Text)]

class ShaderParams a where
  shaderParams :: ShaderParamDeriveOpts -> a -> (ParamFields a, GL.Program -> IO (IO ()))

instance (Generic a, GShaderParams (Rep a)) => ShaderParams a where
  shaderParams opts a = (ParamFields opts fields, init)
    where
      (fields, init) = gShaderParams opts (from a)

--------------------------------------------------------------------------------

data Name (s :: Symbol) = Name

instance (s ~ t) => IsLabel s (Name t) where
  fromLabel = Name

uniform :: forall a s t. (KnownSymbol s, HasField s a t) => ParamFields a -> Name s -> String
uniform (ParamFields opts _) _ = spFieldLabelModifier opts $ symbolVal $ Proxy @s

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

--------------------------------------------------------------------------------

type Vec2 = GL.Vector2 Float
type Vec3 = GL.Vector3 Float
type Vec4 = GL.Vector4 Float
type Color3 = GL.Color3 Float
type Color4 = GL.Color4 Float
type GLint = GL.GLint

class GLSLType t where glslType :: Proxy t -> Text

instance GLSLType GL.GLint where glslType _ = "int"
instance GLSLType Float where glslType _ = "float"
instance GLSLType Double where glslType _ = "double"
instance GLSLType Vec2 where glslType _ = "vec2"
instance GLSLType Vec3 where glslType _ = "vec3"
instance GLSLType Vec4 where glslType _ = "vec4"
instance GLSLType Color3 where glslType _ = "vec3"
instance GLSLType Color4 where glslType _ = "vec4"

vec2_ :: Float -> Float -> Vec2
vec2_ = GL.Vector2

vec2 :: Float -> Float -> Signal Vec2
vec2 x y = pure $ vec2_ x y

vec3_ :: Float -> Float -> Float -> Vec3
vec3_ = GL.Vector3

vec3 :: Float -> Float -> Float -> Signal Vec3
vec3 x y z = pure $ vec3_ x y z

vec4_ :: Float -> Float -> Float -> Float -> Vec4
vec4_ = GL.Vector4

vec4 :: Float -> Float -> Float -> Float -> Signal Vec4
vec4 x y z w = pure $ vec4_ x y z w

color3_ :: Float -> Float -> Float -> Color3
color3_ = GL.Color3

color3 :: Float -> Float -> Float -> Signal Color3
color3 r g b = pure $ color3_ r g b

rgb_ :: Word8 -> Word8 -> Word8 -> Color3
rgb_ r g b = GL.Color3 (c r) (c g) (c b)
  where
    c x = fromIntegral x / 255.0

rgb :: Word8 -> Word8 -> Word8 -> Signal Color3
rgb r g b = pure $ rgb_ r g b

color4_ :: Float -> Float -> Float -> Float -> Color4
color4_ = GL.Color4

color4 :: Float -> Float -> Float -> Float -> Signal Color4
color4 r g b a = pure $ color4_ r g b a

rgba_ :: Word8 -> Word8 -> Word8 -> Word8 -> Color4
rgba_ r g b a = GL.Color4 (c r) (c g) (c b) (c a)
  where
    c x = fromIntegral x / 255.0

rgba :: Word8 -> Word8 -> Word8 -> Word8 -> Signal Color4
rgba r g b a = pure $ rgba_ r g b a

float :: Float -> Signal Float
float = pure

int :: GL.GLint -> Signal GL.GLint
int = pure

--------------------------------------------------------------------------------

origin :: Signal Vec3
origin = vec3 0 0 0

up :: Signal Vec3
up = vec3 0 1 0

down :: Signal Vec3
down = vec3 0 (-1) 0

right :: Signal Vec3
right = vec3 1 0 0

left :: Signal Vec3
left = vec3 (-1) 0 0

forward :: Signal Vec3
forward = vec3 0 0 1

backward :: Signal Vec3
backward = vec3 0 0 (-1)

--------------------------------------------------------------------------------

data TexUniform (n :: Nat) = TexUniform (Maybe GL.TextureObject)

instance KnownNat n => GL.Uniform (TexUniform n) where
  uniform loc = makeStateVar get set
    where
      get = do
        GL.activeTexture $= GL.TextureUnit (fromIntegral $ natVal $ Proxy @n)
        fmap TexUniform $ GL.get $ GL.textureBinding GL.Texture2D
      set (TexUniform tex) = do
        GL.activeTexture $= GL.TextureUnit (fromIntegral $ natVal $ Proxy @n)
        GL.textureBinding GL.Texture2D $= tex
        GL.uniform loc $= GL.TextureUnit (fromIntegral $ natVal $ Proxy @n)

  uniformv location count = GL.uniformv location count . (castPtr :: Ptr (TexUniform n) -> Ptr GLint)

--------------------------------------------------------------------------------

newtype Signal a = Signal (IO (Maybe a))

instance Functor Signal where
  fmap f (Signal a) = Signal (fmap (fmap f) a)

instance Applicative Signal where
  pure a = Signal (pure $ Just a)
  Signal f <*> Signal a = Signal $ do
    f' <- f
    a' <- a
    pure $ f' <*> a'

instance Monad Signal where
  Signal m >>= f = Signal $ do
    m >>= \case
      Just a -> case f a of Signal r -> r
      Nothing -> pure Nothing

signalValue :: MonadIO m => Signal a -> m (Maybe a)
signalValue (Signal v) = liftIO v

signalValueIO :: Signal a -> IO (Maybe a)
signalValueIO (Signal v) = v
