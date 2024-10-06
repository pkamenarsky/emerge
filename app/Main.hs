{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Exception
import Control.Monad (void)
import Control.Monad.IO.Class

import Data.Foldable
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.StateVar
import Data.String.Interpolate (i)
import Data.Void
import qualified Data.Map as M
import qualified Data.Vector.Storable as V

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Common
import Gen
import Types
import Render
import SDF

import Syn
import Syn.Run

import GHC.Word
import GHC.Word

import qualified Sound.RtMidi as RT

import Debug.Trace (traceIO)

--------------------------------------------------------------------------------

type Shader0Params = Params2
  Float -- 10.0
  Float -- 0.5

gptShader0 :: MonadIO m => RectBuffer -> OpOptions -> Signal (Shader0Params Values) -> Syn [Out] m a
gptShader0 rectBuf opts = genShaderSyn rectBuf opts fragT
  where
    fragT uniforms defParams (Params2 p0 p1) = [i|
\#ifdef GL_ES
precision mediump float;
\#endif

#{formatUniforms uniforms}

const int MAX_STEPS = 100;
const float MIN_DIST = 0.001;
const float MAX_DIST = 100.0;

// Gyroid SDF function
float gyroid(vec3 p) {
    return sin(p.x) * cos(p.y) + sin(p.y) * cos(p.z) + sin(p.z) * cos(p.x);
}

// Function to get distance to the object
float map(vec3 p) {
    return gyroid(p * #{p0}) * #{p1} + length(p) - 1.0;  // Combine with sphere for variation
}

// Raymarching function
float raymarch(vec3 ro, vec3 rd) {
    float dO = 0.0;
    for (int i = 0; i < MAX_STEPS; i++) {
        vec3 p = ro + rd * dO;
        float dS = map(p);
        if (dS < MIN_DIST) break;
        dO += dS;
        if (dO > MAX_DIST) break;
    }
    return dO;
}

// Normal calculation
vec3 getNormal(vec3 p) {
    vec2 e = vec2(0.001, 0.0);
    vec3 n = vec3(
        map(p + e.xyy) - map(p - e.xyy),
        map(p + e.yxy) - map(p - e.yxy),
        map(p + e.yyx) - map(p - e.yyx)
    );
    return normalize(n);
}

// Lighting calculation
float getLight(vec3 p) {
    vec3 lightPos = vec3(3.0, 5.0, 5.0);
    vec3 L = normalize(lightPos - p);
    vec3 N = getNormal(p);
    float diff = clamp(dot(N, L), 0.0, 1.0);
    return diff;
}

void main() {
    vec2 uv = (gl_FragCoord.xy - 0.5 * #{u_resolution defParams}.xy) / #{u_resolution defParams}.y;

    vec3 ro = vec3(0.0, 0.0, 5.0);
    vec3 rd = normalize(vec3(uv, -1.5));

    // Raymarch the scene
    float d = raymarch(ro, rd);
    
    // Background color
    vec3 col = vec3(0.0);
    
    // If we hit the object
    if (d < MAX_DIST) {
        vec3 p = ro + rd * d;
        float light = getLight(p);
        col = mix(vec3(0.2, 0.3, 0.7), vec3(0.9, 0.9, 0.9), light);
    }

   gl_FragColor = vec4(col, 1.0);
} |]

type Shader1Params = Params4
  GL.GLint -- 16.0
  Float -- 0.05
  Float -- 10.0
  Float -- 0.3

gptShader1 :: MonadIO m => RectBuffer -> OpOptions -> Signal (Shader1Params Values) -> Syn [Out] m a
gptShader1 rectBuf opts = genShaderSyn rectBuf opts fragT
  where
    fragT uniforms _ (Params4 p0 p1 p2 p3) = [i|
\#ifdef GL_ES
precision mediump float;
\#endif

#{formatUniforms uniforms}

// Function to create circular floral pattern
float circle(vec2 uv, vec2 pos, float radius) {
    return smoothstep(radius, radius + 0.01, length(uv - pos));
}

// Function to create radial petal-like pattern
float petalPattern(vec2 uv, vec2 pos, float radius, int petalCount) {
    float angle = atan(uv.y - pos.y, uv.x - pos.x);
    float petal = sin(float(petalCount) * angle) * 0.5 + 0.5;
    return circle(uv, pos, radius) * petal;
}

// Wave pattern function
float wavePattern(vec2 uv, float frequency, float amplitude) {
    float u_time = 1.0;
    return sin(uv.y * frequency + u_time) * amplitude;
}

void main() {
    vec2 uv = gl_FragCoord.xy / u_resolution.xy;
    uv -= 0.5;  // Centering UV coordinates
    uv.x *= u_resolution.x / u_resolution.y;

    // Create background color
    vec3 color = vec3(0.1, 0.1, 0.1);

    // Create the wave-like pattern
    float wave = wavePattern(uv, #{p2}, #{p3});

    // Create the floral pattern
    float floral = 0.0;
    int petalCount = #{p0};
    float radius = #{p1};
    vec2 gridPos = vec2(mod(uv.x * 5.0, 1.0), mod(uv.y * 5.0, 1.0));

    for (float x = -1.0; x <= 1.0; x += 1.0) {
        for (float y = -1.0; y <= 1.0; y += 1.0) {
            floral += petalPattern(uv, vec2(x, y) + gridPos, radius, petalCount);
        }
    }

    // Combine the wave and floral patterns
    color += vec3(floral * wave);

    gl_FragColor = vec4(color, 1.0);
} |]

--------------------------------------------------------------------------------

scene :: MonadIO m => RectBuffer -> Event () -> Signal (Double, Double) -> Signal (Word8 -> Word8) -> Syn [Out] m a
scene rectBuf mouseClick mousePos cc = do
  -- asum [ void $ circleSyn rectBuf defaultOpOptions (circleParams 0.05), on mouseClick ]
  -- asum [ void $ circleSyn rectBuf defaultOpOptions (circleParams 0.1), on mouseClick ]
  asum [ void $ circleSyn rectBuf defaultOpOptions (circleParams 0.15), on mouseClick ]

  gptShader0 rectBuf defaultOpOptions (Params2 <$> fmap (toRange 1 10) (ccValue 14) <*> fmap (toRange 0 2) (ccValue 15))
  -- gptShader1 rectBuf defaultOpOptions (Params4 <$> ccValue 14 <*> fmap (toRange 0 1) (ccValue 15) <*> fmap (toRange 0 20) (ccValue 16) <*> fmap (toRange 0.1 5) (ccValue 17))

  -- sdfSyn rectBuf defaultOpOptions
  --   (trace traceParams)
  --   (rotate rotateY $ rotate rotateX $ box (pure $ BoxParams (GL.Vector3 0.5 0.5 0.3)))
  -- blendSyn rectBuf defaultBlendOptions (pure $ BlendParamsSyn 0.5)
  --   (fillSyn rectBuf defaultOpOptions fillParams)
  --   (circleSyn rectBuf defaultOpOptions (circleParams 0.15))

  where
    -- traceParams = fmap (\c -> defaultTraceParams { tpMaxIterations = fi c, tpFresnelBase = 1, tpFresnelExp = 2 }) cc
    traceParams :: Signal (TraceParams Values)
    traceParams = TraceParams <$> ccValue 14 <*> fmap (toRange 0.5 2) (ccValue 15) <*> fmap (toRange 1 5) (ccValue 16) <*> fmap (toRange 0 1) (ccValue 17)

    toRange :: Float -> Float -> Word8 -> Float
    toRange mi ma w = mi + (realToFrac w / 127.0 * (ma - mi))

    ccValue :: Num a => Word8 -> Signal a
    ccValue ccId = fmap fromIntegral (cc <*> pure ccId)

    rotateX :: Signal (RotateParams Values)
    rotateX = fmap (\(x, _) -> RotateParams (GL.Vector3 0 1 0) (tf $ x / (-100))) mousePos

    rotateY :: Signal (RotateParams Values)
    rotateY = fmap (\(_, y) -> RotateParams (GL.Vector3 1 0 0) (tf $ y / 100)) mousePos

    fi = fromIntegral
    tf = realToFrac

    fillParams = flip fmap mousePos $ \(mx, my) -> FillParams
      (GL.Color4 (tf $ mx / 1024.0) (tf $ my / 1024.0) 1 1) 

    circleParams radius = flip fmap mousePos $ \(mx, my) -> CircleParams
      (GL.Color4 (tf $ mx / 1024.0) (tf $ my / 1024.0) 1 1) 
      (GL.Vertex2 (tf $ mx / 1024.0) (1 - tf (my / 1024.0)))
      radius

main :: IO ()
main = do
  dev <- RT.defaultInput
  RT.openPort dev 1 "syn"

  ccMap <- newIORef mempty :: IO (IORef (M.Map Word8 Word8))

  RT.setCallback dev $ \ts msg -> do
    putStrLn $ "id: " <> show (msg V.! 1) <> ", value: " <> show (msg V.! 2)
    atomicModifyIORef' ccMap (\m -> (M.insert (msg V.! 1) (msg V.! 2) m, ()))

  _ <- GLFW.init

  GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 6
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core

  bracket
    (GLFW.createWindow 1024 1024 "SYN" Nothing Nothing)
    (traverse_ GLFW.destroyWindow)
    $ \mWin -> do
         let mousePos = Signal $ maybe (pure (0, 0)) GLFW.getCursorPos mWin

         mouseClick <- newEvent

         GLFW.makeContextCurrent mWin
         GL.debugMessageCallback $= Just dbg

         rectBuf <- createRectBuffer
         (blitToScreen, _) <- blit rectBuf (GL.Size 1024 1024)

         for_ mWin (go False mouseClick blitToScreen Nothing $ reinterpret $ scene rectBuf mouseClick mousePos (Signal $ fmap (\ccMap' ccId -> fromMaybe 0 $ M.lookup ccId ccMap') (readIORef ccMap)))

  putStrLn "bye..."

  where
    dbg msg@(GL.DebugMessage _ _ _ severity _) = do
      case severity of
        GL.DebugSeverityNotification -> pure ()
        _ -> traceIO $ show msg

    tf :: Double -> Float
    tf = realToFrac

    clicked :: Bool -> Bool -> Bool
    clicked False True = True
    clicked _ _ = False

    maybeHead :: [a] -> Maybe a
    maybeHead (a:_) = Just a
    maybeHead _ = Nothing

    go :: Bool -> Event () -> (GL.TextureObject -> IO ()) -> Maybe Out -> Run [Out] IO Void -> GLFW.Window -> IO ()
    go mouseButtonSt e@(Event mouseClick) blitToScreen mOut run win = do
      st <- GLFW.getMouseButton win GLFW.MouseButton'1

      mouseButtonSt' <- case st of
        GLFW.MouseButtonState'Pressed -> pure True
        _ -> pure False

      if clicked mouseButtonSt mouseButtonSt'
        then writeIORef mouseClick (Just ())
        else pure ()

      (next, rOut) <- unblock run

      writeIORef mouseClick Nothing

      -- [0]
      (next', rOut') <- unblock next

      let mOut' = asum [rOut' >>= maybeHead, rOut >>= maybeHead, mOut]

      for_ mOut' $ \out -> do
        outRender out
        blitToScreen (outTex out)

      GLFW.swapBuffers win

      esc <- GLFW.getKey win GLFW.Key'Escape

      close <- GLFW.windowShouldClose win

      if close || esc == GLFW.KeyState'Pressed
        then pure ()
        else do
          GLFW.pollEvents
          go mouseButtonSt' e blitToScreen mOut' next' win
