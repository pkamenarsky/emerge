{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Applicative hiding ((<**>))
import Control.Concurrent
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
import Event
import Shader
import Ops
import Types
import SDF

import Syn hiding (forever)
import Syn.Run (Event (Event))
import qualified Syn.Run as Run

-- import Sound.Sc3 hiding (blend)

import GHC.Generics
import GHC.Int
import GHC.Word

import qualified Sound.RtMidi as RT
import Sound.SC3 hiding (blend)
import qualified Sound.SC3.FD as FD
import Sound.SC3.UGen
import Sound.SC3.UGen.Bindings
import Sound.SC3.Server.Command
import qualified Sound.SC3.Server.Transport.FD as FD
import qualified Sound.OSC.Transport.FD as FD
import qualified Sound.OSC.Transport.FD.UDP as FD
import Sound.OSC.Datum
import Sound.OSC.Packet

import Sound.Tidal.Pattern
import qualified Sound.Tidal.Core as T
import qualified Sound.Tidal.Pattern as T

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

pat1 :: Pattern Int
pat1 = T.fromList [1, 2, 3]

pat2 :: Pattern Int
pat2 = T.fromList [4, 5, 6]

-- patternToSig :: Pattern a -> Signal a
patternToSig p = getA <$> events
  where
    arc :: Arc
    arc = Arc 0 10

    getA (T.Event _ctx whole part a) = a
    events = queryArc p arc

scene :: Op a
scene = signals $ \SignalContext {..} -> do
  let cc ccId s e = fmap (toRange s e) (ccRaw ccId)

  let b1 = circle o { radius = fmap (\(x, _) -> tf (x / 1024)) mousePos }
      b2 = circle o { radius = fmap (\(_, y) -> tf (y / 1024)) mousePos }

  let dode1 offset =
          translate o { vec = vec3 -0.5 offset 0 }
        $ rotate o { axis = right, radians = fmap (\(_, y) -> tf (y / 100)) mousePos }
        $ rotate o { axis = up, radians = fmap (\(x, _) -> tf (x / -100)) mousePos }
        $ dodecahedron o { radius = (ranged 0.2 0.3 -1 1 . sin . (* 0.5)) <$> time }

  let dode2 =
          translate o { vec = vec3 0.5 0 0 }
        $ rotate o { axis = right, radians = fmap (\(_, y) -> tf (y / 200)) mousePos }
        $ rotate o { axis = up, radians = fmap (\(x, _) -> tf (x / -200)) mousePos }
        -- $ dodecahedron o { radius = cc 15 0 1 }
        $ dodecahedron o { radius = (ranged 0.3 0.5 -1 1 . sin . (* 0.1)) <$> time }

  let sphere2 =
          translate o { vec = vec3 0.5 0 0 }
        $ sphere o { radius = (ranged 0.3 0.5 -1 1 . sin . (* 0.1)) <$> time }

  let bounce offset = translate o { vec = vec3 0 offset 0 } $ do
        asum [ dode2, on_ leftDown ]
        asum [ sphere2, on_ leftDown ]
        bounce 0.1

  let tr = do
        asum [ view $ trace o { maxIterations = pure 50 }, on_ rightDown ]
        asum [ view $ trace o { maxIterations = pure 3 }, on_ rightDown ]
        tr
        
  -- feedback $ \r -> blend o o { factor = pure 0.05 } r $ grain o { t = (/ 3) <$> time, multiplier = pure 20 }

  grain o { t = (/ 3) <$> time, multiplier = pure 20 }
    $ sdf tr
    $ softUnion_ o { k = cc 16 0.1 10 }
        <$$> softUnions o { k = pure 0.2 }
           [ dode1 ((offset - 2.5) / 5)
           | offset <- [0..5]
           ]
        <**> asum [ bounce 0, bounce 0.1, bounce 0.2, on_ middleDown ]

  feedback $ \r -> blend o o { factor = pure 0.01 } r $ asum [ blend o o b1 b2, on leftDown ]

  feedback $ \r -> blend o o { factor = pure 0.01 } r $ asum
    [ gptShader0 o
        { p0 = fmap (\(x, _) -> ranged 1 10 0 1024 (tf x)) mousePos
        , p1 = fmap (\(_, y) -> ranged 1 10 0 1024 (tf y)) mousePos
        }
    , on leftDown
    ]

  feedback $ \r -> blend o o { factor = pure 0.05 } r $ sdf (view $ trace o { maxIterations = pure 2, clearColor = color3_ <$> cc 14 0 1 <*> cc 15 0 1 <*> cc 16 0 1 })
    $ rotate o { axis = vec3 1 0 0, radians = fmap (\(_, y) -> tf (y / 100)) mousePos }
    $ rotate o { axis = vec3 0 1 0, radians = fmap (\(x, _) -> tf (x / -100)) mousePos }
    $ box o { dimensions = vec3 0.5 0.5 0.3 }

  where
    leftDown = mouseDown GLFW.MouseButton'1
    middleDown = mouseDown GLFW.MouseButton'3
    rightDown = mouseDown GLFW.MouseButton'2

run :: Op Void -> IO ()
run op = do
  GLFW.setErrorCallback $ Just $ \e str -> putStrLn $ show e <> ", " <> str

  _ <- GLFW.init

  GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 6
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core

  bracket
    (GLFW.createWindow 1024 1024 "SYN" Nothing Nothing)
    (traverse_ GLFW.destroyWindow)
    $ \mWin -> handle (print . se) $ do
         GLFW.makeContextCurrent mWin
         GL.debugMessageCallback $= Just dbg

         for_ mWin $ \win -> do
           rectBuf <- createRectBuffer
           (blitToScreen, _) <- blit rectBuf (GL.Size 1024 1024)

           (evtRef, evtCtx) <- eventContext
           (midiDev, ccMap, sigCtx) <- signalContext win

           flip runReaderT (OpContext o rectBuf evtCtx sigCtx) $ loop win evtRef midiDev ccMap (render blitToScreen) (unOp op)

  putStrLn "bye..."

  where
    se :: SomeException -> SomeException
    se = id

    render blitToScreen out = do
      outRender out
      blitToScreen (outTex out)

    dbg msg@(GL.DebugMessage _ _ _ severity _) = do
      case severity of
        GL.DebugSeverityNotification -> pure ()
        _ -> traceIO $ show msg

main :: IO ()
main = run scene

-- h = withSc3 reset
-- 
-- main2 = withSc3 $ do
--   reset
--   let k = sinOsc kr 10 0 * 100
-- 
--   play $ out 0 (sinOsc ar (440 + k) 0 * 0.2)
--   play $ out 1 (sinOsc ar 550 0 * 0.3)


-- -- Define the number of oscillators for Shepard tone (e.g., 10 sine waves spaced by octaves)
-- numOscillators :: Int
-- numOscillators = 10

synthDefs = FD.withSC3 $ \fd -> do
  FD.async fd (d_recv sawSynth)
  -- FD.playSynthdef 1000 fd sawSynth2
  -- FD.play fd $ out 0 $ saw AR 440 * 0.6

synthDefs3 = withSC3 $ do
  r <- async (d_recv sawSynth)
  liftIO $ print r

  fd <- liftIO $ FD.openUDP "127.0.0.1" 57120
  liftIO $ FD.sendMessage fd $ (message "/dirt/add_synth" [ASCII_String $ ascii "saw_wave"])

  liftIO $ threadDelay 10000000000
  -- play $ out 0 $ saw AR 440 * 0.6

synthDefs4 = do
  fd <- FD.openUDP "127.0.0.1" 57120
  FD.sendMessage fd $ (message "/dirt/lalala" [])
  -- threadDelay 100000000000
  where

sawSynth2 :: Synthdef
sawSynth2 = synthdef "saw_wave"
  $ out 0
  $ saw AR 440 * 0.1

sawSynth :: Synthdef
sawSynth = synthdef "saw_wave"
  $ out 0
  $ pan2 (pulse AR (control KR "freq" 440) 0.8 * (control KR "amp" 0.1)) (control KR "pan" 0) 1

synthDefs' = do
  fd <- FD.openUDP "127.0.0.1" 57110
  FD.async fd (d_recv sawSynth)
  -- threadDelay 100000000000
  where

testSC3 = FD.withSC3 $ \fd -> do
    -- Send a simple status message to check if the server is alive
    FD.async fd $ (message "/status" [])

sc3 :: IO ()
sc3 = withSC3 $ do
  play $ out 0 $ sinOsc AR 440 0 * 1
