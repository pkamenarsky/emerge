{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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

import Render
import Syn
import Syn.Run
import Types

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

data Field
data Value

type family P f v where
  P Field f = FieldDef f
  P Value v = v

--------------------------------------------------------------------------------

data BoxParams m = BoxParams
  { bpDimensions :: P m (GL.Vector3 Float)
  } deriving Generic

instance ShaderParam (BoxParams Value)
instance NamedShaderParam (BoxParams Field)

box :: Signal (BoxParams Value) -> SDF
box params = SDF $ \pos -> do
  prefix <- name <$> genName
  out <- genName

  let opts = defaultShaderParamDeriveOpts
        { spFieldLabelModifier = (T.unpack prefix <>)
        }
      (uniforms, np) = namedShaderParam opts :: ([(Text, Text)], BoxParams Field)

  W.tell $ pure $ SDFDef
    { sdfIncludes = ["assets/lygia/sdf/boxSDF.glsl"]
    , sdfUniforms = uniforms
    , sdfDecls = [("float", out, [i|boxSDF(#{name pos}, #{field (bpDimensions np)})|])]
    , sdfSetParams = \program -> do
        set <- flip shaderParam program opts
        pure $ signalValue params >>= set
    }

  pure out

--------------------------------------------------------------------------------

data TranslateParams m = TranslateParams
  { tpTranslate :: P m (GL.Vector3 Float)
  } deriving Generic

instance ShaderParam (RotateParams Value)
instance NamedShaderParam (RotateParams Field)

translate :: Signal (GL.Vector3 Float) -> SDF -> SDF
translate params sdf = SDF $ \pos -> do
  prefix <- name <$> genName
  newPos <- genName

  let opts = defaultShaderParamDeriveOpts
        { spFieldLabelModifier = (T.unpack prefix <>)
        }
      (uniforms, np) = namedShaderParam opts :: ([(Text, Text)], TranslateParams Field)

      tParams :: GL.Vector3 Float -> TranslateParams Value
      tParams = TranslateParams

  W.tell $ pure $ SDFDef
    { sdfIncludes = []
    , sdfUniforms = uniforms
    , sdfDecls = [("vec3", newPos, [i|#{name pos} - #{field (tpTranslate np)}|])]
    , sdfSetParams = \program -> do
        set <- flip shaderParam program opts
        pure $ signalValue params >>= set . tParams
    }

  out <- runSDF sdf newPos

  pure out


--------------------------------------------------------------------------------

data RotateParams m = RotateParams
  { rpAxis :: P m (GL.Vector3 Float)
  , rpRadians :: P m Float
  } deriving Generic

instance ShaderParam (TranslateParams Value)
instance NamedShaderParam (TranslateParams Field)

rotate :: Signal (RotateParams Value) -> SDF -> SDF
rotate params sdf = SDF $ \pos -> do
  prefix <- name <$> genName
  newPos <- genName

  let opts = defaultShaderParamDeriveOpts
        { spFieldLabelModifier = (T.unpack prefix <>)
        }
      (uniforms, np) = namedShaderParam opts :: ([(Text, Text)], RotateParams Field)

  W.tell $ pure $ SDFDef
    { sdfIncludes = ["assets/lygia/math/rotate3d.glsl"]
    , sdfUniforms = uniforms
    , sdfDecls = [("vec3", newPos, [i|#{name pos} * rotate3d(#{field (rpAxis np)}, #{field (rpRadians np)})|])]
    , sdfSetParams = \program -> do
        set <- flip shaderParam program opts
        pure $ signalValue params >>= set
    }

  out <- runSDF sdf newPos

  pure out

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

data TraceParams m = TraceParams
  { tpMaxIterations :: P m GL.GLint
  , tpFresnelBase :: P m Float
  , tpFresnelExp :: P m Float
  , tpMixFactor :: P m Float
  } deriving Generic

instance ShaderParam (TraceParams Value)
instance NamedShaderParam (TraceParams Field)

defaultTraceParams :: TraceParams Value
defaultTraceParams = TraceParams
  { tpMaxIterations = 64
  , tpFresnelBase = 1
  , tpFresnelExp = 5
  , tpMixFactor = 0.5
  }

trace :: Signal (TraceParams Value) -> OpOptions -> SDFEval
trace params opts = SDFEval
  { sdfeIncludes = []
  , sdfeUniforms = uniforms
  , sdfeBody = [i|
in vec2 uv;

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
  vec2 pos = uv - 0.5;
  pos.x *= #{aspectRatio};

  vec3 camPos = vec3(0.0, 0.0, 2.0);
  vec3 ray = normalize(vec3(pos, -1.));

  float t = 0.;
  float tMax = 5.;

  for (int i = 0; i < 64; ++i) {
      if (i >= #{field (tpMaxIterations np)}) break;

      vec3 currentPos = camPos + (t * ray);
      float h = sdf(currentPos);

      if (h < 0.0001 || t > tMax) break;
      t += h;
  }

  vec3 color = vec3(1.0, 0.0, 0.0);

  if (t < tMax) {
    vec3 currentPos = camPos + (t * ray);
    vec3 normal = getNormal(currentPos);
    // float diff = dot(vec3(1.0), normal);

    float fresnel = pow(#{field (tpFresnelBase np)} + dot(ray, normal), #{field (tpFresnelExp np)});

    // color = vec3(dot(ray, normal));
    color = mix(color, vec3(#{field (tpMixFactor np)}), fresnel);
  }

  gl_FragColor = vec4(color, 1.0);
} |]
  , sdfeSetParams = \program -> do
      set <- flip shaderParam program defaultShaderParamDeriveOpts
      pure $ signalValue params >>= set
  }
  where
    aspectRatio :: Float
    aspectRatio = fromIntegral (opWidth opts) / fromIntegral (opHeight opts)

    (uniforms, np) = namedShaderParam defaultShaderParamDeriveOpts :: ([(Text, Text)], TraceParams Field)

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

sdfOp :: RectBuffer -> OpOptions -> (OpOptions -> SDFEval) -> SDF -> IO (Op ())
sdfOp rectBuf opts eval sdf = do
  (tex, bindFBO, destroyFBO) <- createFramebuffer opts
  (attribs, bindShader, destroyShader) <- createShader vertT fragT True

  set <- setParams (saProgram attribs)

  (drawRect, destroyDrawRect) <- createDrawRect rectBuf attribs

  pure $ Op
    { opTex = tex
    , opRender = \_ -> do
        bindFBO
        bindShader ()
        set
        drawRect
    , opDestroy = do
        destroyFBO
        destroyShader
        destroyDrawRect
    }
  where
    vertT = [i|
in vec3 a_pos;
in vec2 a_uv;

varying vec2 uv;

void main() {
  gl_Position = vec4(a_pos, 1.0);
  uv = a_uv;
} |]

    (fragT, setParams) = compile (eval opts) sdf

sdfSyn :: MonadIO m => RectBuffer -> OpOptions -> (OpOptions -> SDFEval) -> SDF -> Syn [Out] m a
sdfSyn rectBuf opts eval sdf = do
  Op tex render destroy <- unsafeNonBlockingIO $ sdfOp rectBuf opts eval sdf

  finalize (liftIO destroy) $ view $ pure $ Out
    { outTex = tex
    , outRender = render ()
    }

--------------------------------------------------------------------------------

testSDF = fst $ compile (trace undefined defaultOpOptions) $ union u1 u1
  where
    u1 = union (box undefined) (translate undefined $ box undefined)
  
