{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Applicative
import Control.Exception
import Control.Monad (forever, when, void)
import Control.Monad.IO.Class
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Foldable
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.StateVar
import Data.String.Interpolate (i)
import qualified Data.Time.Clock.System as Time
import Data.Void
import qualified Data.Map as M
import qualified Data.Vector.Storable as V

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Network.HTTP.Client
-- import Network.HTTP.Client.MultipartFormData

import Common
import Shader
import Ops
import Types
import SDF

import Syn hiding (forever)
import Syn.Run (Event (Event))
import qualified Syn.Run as Run

import GHC.Generics
import GHC.Int
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
gptShader0 = shader0 o fragT
  where
    fragT opts u = [i|
#{formatParamUniforms u}

const int MAX_STEPS = 100;
const float MIN_DIST = 0.001;
const float MAX_DIST = 100.0;

// Gyroid SDF function
float gyroid(vec3 p) {
    return sin(p.x) * cos(p.y) + sin(p.y) * cos(p.z) + sin(p.z) * cos(p.x);
}

// Function to get distance to the object
float map(vec3 p) {
    return gyroid(p * #{uniform u #p0}) * #{uniform u #p1} + length(p) - 1.0;  // Combine with sphere for variation
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

scene :: Manager -> Event () -> Signal Float -> Signal (Double, Double) -> Signal (Word8 -> Word8) -> Op a
scene _manager mouseClick time mousePos ccMap = do
  let b1 = circle o { radius = fmap (\(x, _) -> tf (x / 1024)) mousePos }
      b2 = circle o { radius = fmap (\(_, y) -> tf (y / 1024)) mousePos }

  let dode1 =
          translate o { vec = vec3 -0.5 0 0 }
        $ rotate o { axis = right, radians = fmap (\(_, y) -> tf (y / 100)) mousePos }
        $ rotate o { axis = up, radians = fmap (\(x, _) -> tf (x / -100)) mousePos }
        $ dodecahedron o { radius = (ranged 0.2 0.3 0 1 . abs . sin . (* 7)) <$> time }

  let dode2 =
          translate o { vec = vec3 0.5 0 0 }
        $ rotate o { axis = right, radians = fmap (\(_, y) -> tf (y / 200)) mousePos }
        $ rotate o { axis = up, radians = fmap (\(x, _) -> tf (x / -200)) mousePos }
        -- $ dodecahedron o { radius = cc 15 0 1 }
        $ dodecahedron o { radius = (ranged 0.3 0.5 0 1 . abs . sin . (* 3)) <$> time }

  let sphere2 =
          translate o { vec = vec3 0.5 0 0 }
        $ sphere o { radius = (ranged 0.3 0.5 0 1 . abs . sin . (* 3)) <$> time }

  let bounce offset = translate o { vec = vec3 0 offset 0 } $ do
        asum [ dode2, Run.on mouseClick ]
        asum [ sphere2, Run.on mouseClick ]
        bounce (offset + 0.1)

  -- feedback $ \r -> blend o o { factor = pure 0.05 } r $ grain o { t = (/ 3) <$> time, multiplier = pure 20 }
  grain o { t = (/ 3) <$> time, multiplier = pure 20 }
    $ sdf (trace o { maxIterations = pure 50 })
    $ softUnion o { k = cc 16 0.1 10 }
        -- (softUnion o (plane o { normal = pure $ vec3 0 0 (1), planePoint = vec3 <$> pure 0 <*> pure 0 <*> cc 17 (-1) 1 }) dode1)
        dode1
        (bounce 0)

  feedback $ \r -> blend o o { factor = pure 0.01 } r $ asum [ blend o o b1 b2, Main.on mouseClick ]

  feedback $ \r -> blend o o { factor = pure 0.01 } r $ asum
    [ gptShader0 o
        { p0 = fmap (\(x, _) -> ranged 1 10 0 1024 (tf x)) mousePos
        , p1 = fmap (\(_, y) -> ranged 1 10 0 1024 (tf y)) mousePos
        }
    , Main.on mouseClick
    ]

  feedback $ \r -> blend o o { factor = pure 0.05 } r $ sdf (trace o { maxIterations = pure 2, clearColor = color3_ <$> cc 14 0 1 <*> cc 15 0 1 <*> cc 16 0 1 })
    $ rotate o { axis = vec3 1 0 0, radians = fmap (\(_, y) -> tf (y / 100)) mousePos }
    $ rotate o { axis = vec3 0 1 0, radians = fmap (\(x, _) -> tf (x / -100)) mousePos }
    $ box o { dimensions = vec3 0.5 0.5 0.3 }

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

  _ <- GLFW.init

  GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 6
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core

  t0 <- Time.getSystemTime

  let time = Signal $ do
        now <- Time.getSystemTime

        let s = Time.systemSeconds now - Time.systemSeconds t0
        let ns = fi (Time.systemNanoseconds now) - fi (Time.systemNanoseconds t0) :: Int64

        pure $ fi s + fi ns / 1000000000

  bracket
    (GLFW.createWindow 1024 1024 "SYN" Nothing Nothing)
    (traverse_ GLFW.destroyWindow)
    $ \mWin -> do
         let mousePos = Signal $ maybe (pure (0, 0)) GLFW.getCursorPos mWin

         mouseClick <- Run.newEvent

         GLFW.makeContextCurrent mWin
         for_ mWin $ \win -> GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled

         GL.debugMessageCallback $= Just dbg

         rectBuf <- createRectBuffer
         (blitToScreen, _) <- blit rectBuf (GL.Size 1024 1024)

         flip runReaderT (OpContext o rectBuf undefined undefined) $ for_ mWin (go dev ccMap False False mouseClick blitToScreen Nothing $ reinterpret $ runOp $ scene manager mouseClick time mousePos (Signal $ fmap (\ccMap' ccId -> fromMaybe 0 $ M.lookup ccId ccMap') (readIORef ccMap)))

  putStrLn "bye..."

  where
    frameByFrame = False

    dbg msg@(GL.DebugMessage _ _ _ severity _) = do
      case severity of
        GL.DebugSeverityNotification -> pure ()
        _ -> traceIO $ show msg

    clicked :: Bool -> Bool -> Bool
    clicked False True = True
    clicked _ _ = False

    maybeHead :: [a] -> Maybe a
    maybeHead (a:_) = Just a
    maybeHead _ = Nothing

    go :: RT.InputDevice -> IORef (M.Map Word8 Word8) -> Bool -> Bool -> Event () -> (GL.TextureObject -> IO ()) -> Maybe Out -> Run [Out] (ReaderT OpContext IO) Void -> GLFW.Window -> ReaderT OpContext IO ()
    go dev ccMap mouseButtonSt rightSt' e@(Event mouseClick) blitToScreen mOut run win = do
      st <- liftIO $ GLFW.getMouseButton win GLFW.MouseButton'1
      rightSt <- liftIO $ GLFW.getMouseButton win GLFW.MouseButton'2

      mouseButtonSt' <- case st of
        GLFW.MouseButtonState'Pressed -> pure True
        _ -> pure False

      rightSt'' <- case rightSt of
        GLFW.MouseButtonState'Pressed -> pure True
        _ -> pure False

      (_, msg) <- liftIO $ RT.getMessage dev

      when (V.length msg >= 3) $ liftIO $ do
        putStrLn $ "id: " <> show (msg V.! 1) <> ", value: " <> show (msg V.! 2) <> ", ctrl: " <> show (msg V.! 0)
        atomicModifyIORef' ccMap (\m -> (M.insert (msg V.! 1) (msg V.! 2) m, ()))

      (mOut', next') <- if not frameByFrame || clicked rightSt' rightSt''
        then do
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

          pure (mOut', next')
        else pure (mOut, run)

      liftIO $ GLFW.swapBuffers win

      esc <- liftIO $ GLFW.getKey win GLFW.Key'Escape

      close <- liftIO $ GLFW.windowShouldClose win

      if close || esc == GLFW.KeyState'Pressed
        then pure ()
        else do
          liftIO $ GLFW.pollEvents
          go dev ccMap mouseButtonSt' rightSt'' e blitToScreen mOut' next' win
