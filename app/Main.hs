{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Applicative
import Control.Exception
import Control.Monad (void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
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

import Network.HTTP.Client
-- import Network.HTTP.Client.MultipartFormData

import Common
import Gen
import Types
import Render
import SDF

import Syn
import Syn.Run (Event (Event))
import qualified Syn.Run as Run

import GHC.Generics
import GHC.Word

import qualified Sound.RtMidi as RT

import Debug.Trace (traceIO)

postImage :: Manager -> BL.ByteString -> IO B.ByteString
postImage manager png = do
  initialRequest <- parseRequest "http://localhost:5000/upload"
  let request = initialRequest
        { method = "POST"
        , requestBody = RequestBodyLBS png
        , requestHeaders = [("Content-Type", "image/png")]
        }

  response <- httpLbs request manager
  pure $ BL.toStrict $ responseBody response

--------------------------------------------------------------------------------

data S0 = S0 { p0 :: Signal Float, p1 :: Signal Float } deriving Generic
instance Default S0 where def = S0 { p0 = pure 10, p1 = pure 0.5 }

gptShader0 :: S0 -> Op a
gptShader0 = shader0 x fragT
  where
    fragT opts udefs = [i|
#{formatParamUniforms udefs}

const int MAX_STEPS = 100;
const float MIN_DIST = 0.001;
const float MAX_DIST = 100.0;

// Gyroid SDF function
float gyroid(vec3 p) {
    return sin(p.x) * cos(p.y) + sin(p.y) * cos(p.z) + sin(p.z) * cos(p.x);
}

// Function to get distance to the object
float map(vec3 p) {
    return gyroid(p * #{uniform udefs #p0}) * #{uniform udefs #p1} + length(p) - 1.0;  // Combine with sphere for variation
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
    vec2 uv = (gl_FragCoord.xy - 0.5 * #{resVec2 opts}.xy) / #{resVec2 opts}.y;

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
        col = mix(vec3(0.01, 0.01, 0.01), vec3(0.5, 0.5, 0.5), light);
    }

   gl_FragColor = vec4(col, 1.0);
} |]

--------------------------------------------------------------------------------

on :: Event a -> Op a
on = Op . Run.on

scene :: Manager -> Event () -> Signal (Double, Double) -> Signal (Word8 -> Word8) -> Op a
scene manager mouseClick mousePos ccMap = do
  let b1 = circle x { radius = fmap (\(x, y) -> tf (x / 1024)) mousePos }
      b2 = circle x { radius = fmap (\(x, y) -> tf (y / 1024)) mousePos }

  asum [ blend x x b1 b2, on mouseClick ]

  -- gptShader0 rectBuf defaultOpOptions $ GenSignal
  --   ( #p0 =: cc 14 1 10
  --   , #p1 =: cc 15 0 2
  --   )

  asum
    [ gptShader0 x
        { p0 = fmap (\(x, _) -> ranged 1 10 0 1024 (tf x)) mousePos
        , p1 = fmap (\(_, y) -> ranged 1 10 0 1024 (tf y)) mousePos
        }
    , on mouseClick
    ]

  sdf (trace x { maxIterations = ccValue 14 })
    $ rotate x { axis = pure $ vec3 1 0 0, radians = fmap (\(_, y) -> tf (y / 100)) mousePos }
    $ rotate x { axis = pure $ vec3 0 1 0, radians = fmap (\(x, _) -> tf (x / (-100))) mousePos }
    $ box x { dimensions = pure $ vec3 0.5 0.5 0.3 }

  where
    ranged :: Float -> Float -> Float -> Float -> Float -> Float
    ranged a b c d x = a + (b - a) * ((x - c) / (d - c))

    toRange :: Float -> Float -> Word8 -> Float
    toRange mi ma w = mi + (realToFrac w / 127.0 * (ma - mi))

    cc :: Word8 -> Float -> Float -> Signal Float
    cc ccId i a = fmap (toRange i a) (ccValue ccId)

    ccValue :: Num a => Word8 -> Signal a
    ccValue ccId = fmap fromIntegral (ccMap <*> pure ccId)

main :: IO ()
main = do
  dev <- RT.defaultInput
  RT.openPort dev 1 "syn"

  ccMap <- newIORef mempty :: IO (IORef (M.Map Word8 Word8))

  manager <- newManager defaultManagerSettings

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

         mouseClick <- Run.newEvent

         GLFW.makeContextCurrent mWin
         GL.debugMessageCallback $= Just dbg

         rectBuf <- createRectBuffer
         (blitToScreen, _) <- blit rectBuf (GL.Size 1024 1024)

         flip runReaderT (OpContext x rectBuf) $ for_ mWin (go False mouseClick blitToScreen Nothing $ reinterpret $ runOp $ scene manager mouseClick mousePos (Signal $ fmap (\ccMap' ccId -> fromMaybe 0 $ M.lookup ccId ccMap') (readIORef ccMap)))

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

    go :: Bool -> Event () -> (GL.TextureObject -> IO ()) -> Maybe Out -> Run [Out] (ReaderT OpContext IO) Void -> GLFW.Window -> ReaderT OpContext IO ()
    go mouseButtonSt e@(Event mouseClick) blitToScreen mOut run win = do
      st <- liftIO $ GLFW.getMouseButton win GLFW.MouseButton'1

      mouseButtonSt' <- case st of
        GLFW.MouseButtonState'Pressed -> pure True
        _ -> pure False

      if clicked mouseButtonSt mouseButtonSt'
        then liftIO $ writeIORef mouseClick (Just ())
        else pure ()

      (next, rOut) <- unblock run

      liftIO $ writeIORef mouseClick Nothing

      -- [0]
      (next', rOut') <- unblock next

      let mOut' = asum [rOut' >>= maybeHead, rOut >>= maybeHead, mOut]

      liftIO $ for_ mOut' $ \out -> do
        outRender out
        blitToScreen (outTex out)

      liftIO $ GLFW.swapBuffers win

      esc <- liftIO $ GLFW.getKey win GLFW.Key'Escape

      close <- liftIO $ GLFW.windowShouldClose win

      if close || esc == GLFW.KeyState'Pressed
        then pure ()
        else do
          liftIO $ GLFW.pollEvents
          go mouseButtonSt' e blitToScreen mOut' next' win
