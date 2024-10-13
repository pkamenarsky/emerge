{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module SDF where

import Control.Applicative
import Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as ST
import           Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.Writer.CPS as W

import Data.Foldable (for_)
import Data.IORef
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Graphics.Rendering.OpenGL as GL

import GHC.Generics (Generic)

import Common
import Syn
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

-- TODO: those instances are here only to enable event handling in `asum` expressions etc
instance Semigroup SDFEval where
  a <> b = SDFEval
    { sdfeIncludes = sdfeIncludes a <> sdfeIncludes b
    , sdfeUniforms = sdfeUniforms a <> sdfeUniforms b
    , sdfeBody = sdfeBody a <> sdfeBody b
    , sdfeSetParams = \p -> do
        sa <- sdfeSetParams a p
        sb <- sdfeSetParams b p
        pure $ sa >> sb
    }

instance Monoid SDFEval where
  mempty = SDFEval
    { sdfeIncludes = []
    , sdfeUniforms = []
    , sdfeBody = ""
    , sdfeSetParams = \_ -> pure $ pure ()
    }

data SDFDef = SDFDef
  { sdfIncludes  :: [Text]
  , sdfUniforms  :: [(Text, Text)]
  , sdfDecls     :: [(Text, Name, Text)]
  , sdfSetParams :: GL.Program -> IO (IO ())
  }

newtype SDF = SDF { runSDF :: Pos -> W.WriterT [SDFDef] (ST.State Name) Pos }

instance Semigroup SDF where
  (<>) = union'

infinity :: Name
infinity = Name 999999

instance Monoid SDF where
  mempty = SDF $ \_ -> pure infinity

genName :: W.WriterT [SDFDef] (ST.State Name) Name
genName = lift $ ST.state $ \(Name n) -> (Name n, Name (n + 1))

name :: Name -> Text
name (Name n) = "n_" <> T.pack (show n)

-- primitives ------------------------------------------------------------------

data BoxUniforms = BoxUniforms
  { dimensions :: Signal Vec3
  } deriving Generic

instance Default BoxUniforms where
  def = BoxUniforms { dimensions = vec3 0.5 0.5 0.5 }

box' :: BoxUniforms -> SDF
box' params = SDF $ \pos -> do
  prefix <- name <$> genName
  out <- genName

  let opts = def { spFieldLabelModifier = (T.unpack prefix <>) }
      (u, setParams) = shaderParams opts params

  W.tell $ pure $ SDFDef
    { sdfIncludes = ["assets/lygia/sdf/boxSDF.glsl"]
    , sdfUniforms = paramUniforms u
    , sdfDecls = [("float", out, [i|boxSDF(#{name pos}, #{uniform u #dimensions})|])]
    , sdfSetParams = setParams
    }

  pure out

box :: BoxUniforms -> Syn SDF m a
box = view . box'

data PlaneUniforms = PlaneUniforms
  { planePoint :: Signal Vec3
  , normal :: Signal Vec3
  } deriving Generic

instance Default PlaneUniforms where
  def = PlaneUniforms
    { planePoint = vec3 0 0 0
    , normal = vec3 0 0 1
    }

plane' :: PlaneUniforms -> SDF
plane' params = SDF $ \pos -> do
  prefix <- name <$> genName
  out <- genName

  let opts = def { spFieldLabelModifier = (T.unpack prefix <>) }
      (u, setParams) = shaderParams opts params

  W.tell $ pure $ SDFDef
    { sdfIncludes = ["assets/lygia/sdf/planeSDF.glsl"]
    , sdfUniforms = paramUniforms u
    , sdfDecls = [("float", out, [i|planeSDF(#{name pos}, #{uniform u #planePoint}, #{uniform u #normal})|])]
    , sdfSetParams = setParams
    }

  pure out

plane :: PlaneUniforms -> Syn SDF m a
plane = view . plane'

data DodecahedronUniforms = DodecahedronUniforms
  { radius :: Signal Float
  } deriving Generic

instance Default DodecahedronUniforms where
  def = DodecahedronUniforms
    { radius = pure 0.5
    }

dodecahedron' :: DodecahedronUniforms -> SDF
dodecahedron' params = SDF $ \pos -> do
  prefix <- name <$> genName
  out <- genName

  let opts = def { spFieldLabelModifier = (T.unpack prefix <>) }
      (u, setParams) = shaderParams opts params

  W.tell $ pure $ SDFDef
    { sdfIncludes = ["assets/lygia/sdf/dodecahedronSDF.glsl"]
    , sdfUniforms = paramUniforms u
    , sdfDecls = [("float", out, [i|dodecahedronSDF(#{name pos}, #{uniform u #radius})|])]
    , sdfSetParams = setParams
    }

  pure out

dodecahedron :: DodecahedronUniforms -> Syn SDF m a
dodecahedron = view . dodecahedron'

data SphereUniforms = SphereUniforms
  { radius :: Signal Float
  } deriving Generic

instance Default SphereUniforms where
  def = SphereUniforms
    { radius = pure 0.5
    }

sphere' :: SphereUniforms -> SDF
sphere' params = SDF $ \pos -> do
  prefix <- name <$> genName
  out <- genName

  let opts = def { spFieldLabelModifier = (T.unpack prefix <>) }
      (u, setParams) = shaderParams opts params

  W.tell $ pure $ SDFDef
    { sdfIncludes = ["assets/lygia/sdf/sphereSDF.glsl"]
    , sdfUniforms = paramUniforms u
    , sdfDecls = [("float", out, [i|sphereSDF(#{name pos}, #{uniform u #radius})|])]
    , sdfSetParams = setParams
    }

  pure out

sphere :: SphereUniforms -> Syn SDF m a
sphere = view . sphere'

-- transforms ------------------------------------------------------------------

data TranslateUniforms = TranslateUniforms
  { vec :: Signal Vec3
  } deriving Generic

instance Default TranslateUniforms where
  def = TranslateUniforms { vec = origin }

translate' :: TranslateUniforms -> SDF -> SDF
translate' params sdf = SDF $ \pos -> do
  prefix <- name <$> genName
  newPos <- genName

  let opts = def { spFieldLabelModifier = (T.unpack prefix <>) }
      (u, setParams) = shaderParams opts params

  W.tell $ pure $ SDFDef
    { sdfIncludes = []
    , sdfUniforms = paramUniforms u
    , sdfDecls = [("vec3", newPos, [i|#{name pos} - #{uniform u #vec}|])]
    , sdfSetParams = setParams
    }

  runSDF sdf newPos

translate :: Applicative m => TranslateUniforms -> Syn SDF m a -> Syn SDF m a
translate = mapView . translate'

data RotateUniforms = RotateUniforms
  { axis :: Signal Vec3
  , radians :: Signal Float
  } deriving Generic

instance Default RotateUniforms where
  def = RotateUniforms { axis = vec3 1 0 0, radians = pure 0 }

rotate' :: RotateUniforms -> SDF -> SDF
rotate' params sdf = SDF $ \pos -> do
  prefix <- name <$> genName
  newPos <- genName

  let opts = def { spFieldLabelModifier = (T.unpack prefix <>) }
      (u, setParams) = shaderParams opts params

  W.tell $ pure $ SDFDef
    { sdfIncludes = ["assets/lygia/math/rotate3d.glsl"]
    , sdfUniforms = paramUniforms u
    , sdfDecls = [("vec3", newPos, [i|#{name pos} * rotate3d(#{uniform u #axis}, #{uniform u #radians})|])]
    , sdfSetParams = setParams
    }

  runSDF sdf newPos

rotate :: Applicative m => RotateUniforms -> Syn SDF m a -> Syn SDF m a
rotate = mapView . rotate'

-- csg -------------------------------------------------------------------------

union' :: SDF -> SDF -> SDF
union' sdfA sdfB = SDF $ \pos -> do
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

union :: Applicative m => Syn SDF m a -> Syn SDF m a -> Syn SDF m a
union s t = apViewOr (mapView union' s) t

data SoftUnionUniforms = SoftUnionUniforms
  { k :: Signal Float
  } deriving Generic

instance Default SoftUnionUniforms where
  def = SoftUnionUniforms { k = pure 0.1 }

softUnion' :: SoftUnionUniforms -> SDF -> SDF -> SDF
softUnion' params sdfA sdfB = SDF $ \pos -> do
  prefix <- name <$> genName
  pA <- runSDF sdfA pos
  pB <- runSDF sdfB pos

  let opts = def { spFieldLabelModifier = (T.unpack prefix <>) }
      (u, setParams) = shaderParams opts params

  newPos <- genName

  W.tell $ pure $ SDFDef
    { sdfIncludes = ["assets/lygia/sdf/opUnion.glsl"]
    , sdfUniforms = paramUniforms u
    , sdfDecls = [("float", newPos, [i|opUnion(#{name pA}, #{name pB}, #{uniform u #k})|])]
    , sdfSetParams = setParams
    }

  pure newPos

softUnion :: Applicative m => SoftUnionUniforms -> Syn SDF m a -> Syn SDF m a -> Syn SDF m a
softUnion params s t = apViewOr (mapView (softUnion' params) s) t

--------------------------------------------------------------------------------

data TraceUniforms = TraceUniforms
  { maxIterations :: Signal GLint
  , fresnelBase :: Signal Float
  , fresnelExp :: Signal Float
  , mixFactor :: Signal Float
  , clearColor :: Signal Color3
  } deriving Generic

instance Default TraceUniforms where
  def = TraceUniforms
    { maxIterations = pure 64
    , fresnelBase = pure 1
    , fresnelExp = pure 5
    , mixFactor = pure 0.5
    , clearColor = color3 0 0 0
    }

trace
  :: TraceUniforms
  -> OpOptions
  -> SDFEval
trace params opts = SDFEval
  { sdfeIncludes = []
  , sdfeUniforms = paramUniforms u
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
  vec2 uv = gl_FragCoord.xy / #{resVec2 opts};

  vec2 pos = uv - 0.5;
  pos.x *= #{aspectRatio};

  vec3 camPos = vec3(0.0, 0.0, 2.0);
  vec3 ray = normalize(vec3(pos, -1.));

  float t = 0.;
  float tMax = 5.;

  for (int i = 0; i < 64; ++i) {
      if (i >= #{uniform u #maxIterations}) break;

      vec3 currentPos = camPos + (t * ray);
      float h = sdf(currentPos);

      if (h < 0.0001 || t > tMax) break;
      t += h;
  }

  vec3 color = #{uniform u #clearColor};

  if (t < tMax) {
    vec3 currentPos = camPos + (t * ray);
    vec3 normal = getNormal(currentPos);
    // float diff = dot(vec3(1.0), normal);

    float fresnel = pow(#{uniform u #fresnelBase} + dot(ray, normal), #{uniform u #fresnelExp});

    // color = vec3(dot(ray, normal));
    color = mix(color, vec3(#{uniform u #mixFactor}), fresnel);
  }

  gl_FragColor = vec4(color, 1.0);
} |]
  , sdfeSetParams = setParams
  }
  where
    aspectRatio :: Float
    aspectRatio = fi (opWidth opts) / fi (opHeight opts)

    (u, setParams) = shaderParams o params

--------------------------------------------------------------------------------

compile :: SDFEval -> SDF -> (Text, GL.Program -> IO (IO ()))
compile eval sdf = (compileDefs, setParams)
  where
    pos0 = Name 0
    pos1 = Name 1

    (posn, defs) = flip ST.evalState pos1 $ W.runWriterT $ (runSDF sdf pos0)

    compileDefs = T.intercalate "\n" $ mconcat $
      [ [ [i|float #{name infinity} = 99999999.9;|] ]
      , [ [i|\#include "#{include}"|]
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

sdf' :: (OpOptions -> SDFEval) -> SDF -> Op a
sdf' eval sdfDefs = Op $ do
  OpContext opts rectBuf _ _ <- lift ask

  let (fragT, init) = compile (eval opts) sdfDefs

  (out, destroy) <- unsafeNonBlockingIO $ do
    (tex, bindFBO, destroyFBO) <- createFramebuffer opts
    (attribs, bindShader, destroyShader) <- createShader Nothing fragT

    set <- init (saProgram attribs)

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

sdf :: Syn (OpOptions -> SDFEval) (ReaderT OpContext IO) a -> Syn SDF (ReaderT OpContext IO) a -> Op a
sdf eval sdfDefs = Op $ do
  OpContext opts rectBuf _ _ <- lift ask

  (fbo, ref) <- unsafeNonBlockingIO $ do
    ref <- newIORef Nothing
    fbo <- createFramebuffer opts
    pure (fbo, ref)

  finalize (destroy fbo ref) $ apViewOrM (mapViewM (f opts fbo rectBuf ref) eval) sdfDefs

  where
    destroy (_, _, destroyFBO) ref = liftIO $ do
      st <- readIORef ref

      for_ st $ \(_, (_, _, destroyShader), _, (_, destroyDrawRect)) -> do
        destroyFBO
        destroyShader
        destroyDrawRect

    -- TODO: we're comparing shader sources here every frame, probably not the most efficient
    f opts fbo@(tex, bindFBO, _) rectBuf ref evalDef = pure $ \sdfDef -> liftIO $ do
      st <- readIORef ref

      for_ st $ \(_, (_, _, destroyShader), _, (_, destroyDrawRect)) -> do
        destroyShader
        destroyDrawRect

      let (fragT, init) = compile (evalDef opts) sdfDef

      shSt@(attribs, bindShader, _) <- createShader Nothing fragT
      rectSt@(drawRect, _) <- createDrawRect rectBuf attribs

      set <- init (saProgram attribs)

      writeIORef ref $ Just (fragT, shSt, set, rectSt)

      pure
        [ Out
          { outRender = do
              bindFBO
              bindShader
              set
              drawRect
          , outTex = tex
          }
        ]
