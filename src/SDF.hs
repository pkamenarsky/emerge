{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module SDF where

import Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as ST
import qualified Control.Monad.Trans.Writer.CPS as W

import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Graphics.Rendering.OpenGL as GL

import GHC.Generics (Generic)

import Common
import Gen
import Syn
import Syn.Run
import Types hiding (Name)

--------------------------------------------------------------------------------

newtype Name = Name Int
type Pos = Name

data SDFEval = SDFEval
  { sdfeIncludes  :: [Text]
  , sdfeUniforms  :: [(Text, Text)]
  , sdfeBody      :: Text
  , sdfeSetParams :: GL.Program -> IO (IO ())
  }

data SDFDef = SDFDef
  { sdfIncludes  :: [Text]
  , sdfUniforms  :: [(Text, Text)]
  , sdfDecls     :: [(Text, Name, Text)]
  , sdfSetParams :: GL.Program -> IO (IO ())
  }

newtype SDF = SDF { runSDF :: Pos -> W.WriterT [SDFDef] (ST.State Name) Pos }

genName :: W.WriterT [SDFDef] (ST.State Name) Name
genName = lift $ ST.state $ \(Name n) -> (Name n, Name (n + 1))

name :: Name -> Text
name (Name n) = "n_" <> T.pack (show n)

--------------------------------------------------------------------------------

{-

box :: GenSignal '[Param "dimensions" Vec3] -> SDF
box (GenSignal params) = SDF $ \pos -> do
  prefix <- name <$> genName
  out <- genName

  let opts = defaultShaderParamDeriveOpts
        { spFieldLabelModifier = (T.unpack prefix <>)
        }
      (udefs, getUniforms) = shaderParams'' opts $ #dimensions =: vec3 0.5 0.5 0.5

  W.tell $ pure $ SDFDef
    { sdfIncludes = ["assets/lygia/sdf/boxSDF.glsl"]
    , sdfUniforms = paramUniforms udefs
    , sdfDecls = [("float", out, [i|boxSDF(#{name pos}, #{field udefs #dimensions})|])]
    , sdfSetParams = \program -> do
        uniforms <- getUniforms program
        pure $ seqParams (fromTuples params) >>= set uniforms
    }

  pure out

--------------------------------------------------------------------------------

translate :: GenSignal '[Param "translate" Vec3] -> SDF -> SDF
translate (GenSignal params) sdf = SDF $ \pos -> do
  prefix <- name <$> genName
  newPos <- genName

  let opts = defaultShaderParamDeriveOpts
        { spFieldLabelModifier = (T.unpack prefix <>)
        }
      (udefs, getUniforms) = shaderParams'' opts $ #translate =: vec3 0 0 0

  W.tell $ pure $ SDFDef
    { sdfIncludes = []
    , sdfUniforms = paramUniforms udefs
    , sdfDecls = [("vec3", newPos, [i|#{name pos} - #{field udefs #translate}|])]
    , sdfSetParams = \program -> do
        uniforms <- getUniforms program
        pure $ seqParams (fromTuples params) >>= set uniforms
    }

  runSDF sdf newPos

--------------------------------------------------------------------------------

rotate :: GenSignal '[Param "axis" Vec3, Param "radians" Float] -> SDF -> SDF
rotate (GenSignal params) sdf = SDF $ \pos -> do
  prefix <- name <$> genName
  newPos <- genName

  let opts = defaultShaderParamDeriveOpts
        { spFieldLabelModifier = (T.unpack prefix <>)
        }
      (udefs, getUniforms) = shaderParams'' opts
        ( #axis =: vec3 0 0 0
        , #radians =: float 0
        )

  W.tell $ pure $ SDFDef
    { sdfIncludes = ["assets/lygia/math/rotate3d.glsl"]
    , sdfUniforms = paramUniforms udefs
    , sdfDecls = [("vec3", newPos, [i|#{name pos} * rotate3d(#{field udefs #axis}, #{field udefs #radians})|])]
    , sdfSetParams = \program -> do
        uniforms <- getUniforms program
        pure $ seqParams (fromTuples params) >>= set uniforms
    }

  runSDF sdf newPos

--------------------------------------------------------------------------------

union :: SDF -> SDF -> SDF
union sdfA sdfB = SDF $ \pos -> do
  pA <- runSDF sdfA pos
  pB <- runSDF sdfB pos

  newPos <- genName

  W.tell $ pure $ SDFDef
    { sdfIncludes = []
    , sdfUniforms = []
    , sdfDecls = [("float", newPos, [i|min(#{name pA}, #{name pB})|])]
    , sdfSetParams = \_ -> pure (pure ())
    }

  pure newPos

--------------------------------------------------------------------------------

trace
  :: GenSignal
      '[ Param "max_iterations" GLint
       , Param "fresnel_base" Float
       , Param "fresnel_exp" Float
       , Param "mix_factor" Float
       ]
  -> OpOptions
  -> SDFEval
trace (GenSignal params) opts = SDFEval
  { sdfeIncludes = []
  , sdfeUniforms = paramUniforms udefs
  , sdfeBody = [i|
vec3 getNormal(vec3 pos) {
  const float eps = 0.0001;
  const vec2 h = vec2(1., -1.);

  return normalize(
    h.xyy * sdf(pos + h.xyy * eps) +
    h.yyx * sdf(pos + h.yyx * eps) +
    h.yxy * sdf(pos + h.yxy * eps) +
    h.xxx * sdf(pos + h.xxx * eps));
}

void main () {
  vec2 uv = gl_FragCoord.xy / vec2(#{opWidth opts}, #{opHeight opts});

  vec2 pos = uv - 0.5;
  pos.x *= #{aspectRatio};

  vec3 camPos = vec3(0.0, 0.0, 2.0);
  vec3 ray = normalize(vec3(pos, -1.));

  float t = 0.;
  float tMax = 5.;

  for (int i = 0; i < 64; ++i) {
      if (i >= #{field udefs #max_iterations}) break;

      vec3 currentPos = camPos + (t * ray);
      float h = sdf(currentPos);

      if (h < 0.0001 || t > tMax) break;
      t += h;
  }

  vec3 color = vec3(0.0, 0.0, 0.0);

  if (t < tMax) {
    vec3 currentPos = camPos + (t * ray);
    vec3 normal = getNormal(currentPos);
    // float diff = dot(vec3(1.0), normal);

    float fresnel = pow(#{field udefs #fresnel_base} + dot(ray, normal), #{field udefs #fresnel_exp});

    // color = vec3(dot(ray, normal));
    color = mix(color, vec3(#{field udefs #mix_factor}), fresnel);
  }

  gl_FragColor = vec4(color, 1.0);
} |]
  , sdfeSetParams = \program -> do
      uniforms <- getUniforms program
      pure $ seqParams (fromTuples params) >>= set uniforms
  }
  where
    aspectRatio :: Float
    aspectRatio = fi (opWidth opts) / fi (opHeight opts)

    (udefs, getUniforms) = shaderParams'' defaultShaderParamDeriveOpts
      ( #max_iterations =: int 64
      , #fresnel_base =: float 1
      , #fresnel_exp =: float 5
      , #mix_factor =: float 0.5
      )

--------------------------------------------------------------------------------

compile :: SDFEval -> SDF -> (Text, GL.Program -> IO (IO ()))
compile eval sdf = (compileDefs, setParams)
  where
    pos0 = Name 0
    pos1 = Name 1

    (posn, defs) = flip ST.evalState pos1 $ W.runWriterT $ (runSDF sdf pos0)

    compileDefs = T.intercalate "\n" $ mconcat $
      [ [ [i|\#include "#{include}"|]
        | includes <- fmap sdfIncludes defs <> [ sdfeIncludes eval ]
        , include <- includes
        ]
      , [""]
      , [ [i|uniform #{ut} #{un};|]
        | uniforms <- fmap sdfUniforms defs <> [ sdfeUniforms eval ]
        , (ut, un) <- uniforms
        ]
      , [""]
      , [ [i|float sdf(vec3 #{name pos0}) { |] ]
      , [ [i|  #{dt} #{name dn} = #{dr};|]
        | decls <- fmap sdfDecls defs
        , (dt, dn, dr) <- decls
        ]
      , [ [i|  return #{name posn};|] ]
      , [ "}" ]
      , [""]
      , [ sdfeBody eval ]
      ]

    setParams program = do
      set <- sequence
        [ setParams' program
        | setParams' <- fmap sdfSetParams defs <> [ sdfeSetParams eval ]
        ]

      pure $ sequence_ set

--------------------------------------------------------------------------------

sdf :: MonadIO m => RectBuffer -> OpOptions -> (OpOptions -> SDFEval) -> SDF -> Syn [Out] m a
sdf rectBuf opts eval sdf = do
  (out, destroy) <- unsafeNonBlockingIO $ do
    (tex, bindFBO, destroyFBO) <- createFramebuffer opts
    (attribs, bindShader, destroyShader) <- createShader Nothing fragT

    set <- setParams (saProgram attribs)

    (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

    pure
      ( Out
          { outTex = tex
          , outRender = do
              bindFBO
              bindShader
              set
              drawRect
          }
      , do
         destroyFBO
         destroyShader
         destroyDrawRect
      )

  finalize (liftIO destroy) $ view [out]
  where
    (fragT, setParams) = compile (eval opts) sdf

-}
