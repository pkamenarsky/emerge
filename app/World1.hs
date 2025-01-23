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

module World1 (world1) where

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

import Common
import Event
import Shader
import Ops
import Types
import SDF

import Syn hiding (forever)
import Syn.Run (Event (Event))
import qualified Syn.Run as Run

import GHC.Generics

import Debug.Trace (traceIO)

import Prelude hiding (subtract)

--------------------------------------------------------------------------------

data S0 = S0 { p0 :: Signal Float, p1 :: Signal Float } deriving Generic
instance Default S0 where def = S0 { p0 = pure 10, p1 = pure 0.5 }

--------------------------------------------------------------------------------

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
        asum [ view $ traceOrtho o { maxIterations = pure 3 }, on_ rightDown ]
        tr
        
  -- feedback $ \r -> blend o o { factor = pure 0.05 } r $ grain o { t = (/ 3) <$> time, multiplier = pure 20 }

  -- grain o { t = (/ 3) <$> time, multiplier = pure 20 }
  sdf tr
    $ softUnion_ o { k = cc 16 0.1 10 }
        <$$> softUnions o { k = pure 0.2 }
           [ dode1 ((offset - 2.5) / 5)
           | offset <- [0..5]
           ]
        <**> asum [ bounce 0, bounce 0.1, bounce 0.2, on_ middleDown ]

  feedback $ \r -> blend o o { factor = pure 0.01 } r $ asum [ blend o o b1 b2, on leftDown ]

  feedback $ \r -> blend o o { factor = pure 0.05 } r $ sdf (view $ trace o { maxIterations = pure 2, clearColor = color3_ <$> cc 14 0 1 <*> cc 15 0 1 <*> cc 16 0 1 })
    $ rotate o { axis = vec3 1 0 0, radians = fmap (\(_, y) -> tf (y / 100)) mousePos }
    $ rotate o { axis = vec3 0 1 0, radians = fmap (\(x, _) -> tf (x / -100)) mousePos }
    $ box o { dimensions = vec3 0.5 0.5 0.3 }

  where
    leftDown = mouseDown GLFW.MouseButton'1
    middleDown = mouseDown GLFW.MouseButton'3
    rightDown = mouseDown GLFW.MouseButton'2

scene2 :: Op a
scene2 = signals $ \SignalContext {..} -> do
  let pos = fmap (\(x, y) -> vec3_ (x / 100.0) 0 0) mousePos

  sdf (view $ trace o { maxDistance = pure 20, clearColor = color3 0.1 0 0.2, baseColor = color3 0.2 0.2 0.2 }) $ translate o { vec = pos } b -- $ unions
    -- [ translate o { vec = vec3 0 (y * u) 0 } b
    -- | y <- [0..2]
    -- ]

  where
    u = 0.2
    b = subtract (box o { dimensions = vec3 u u u }) c
    -- b = translate o { vec = vec3 1 0 -3 } $ rotate o { axis = right, radians = pure (degrees 90) } $ cylinder o { height = pure 0.2, radius = pure 0.5 }
    c = translate o { vec = vec3 0 -u 0 } $ rotate o { axis = right, radians = pure (degrees 90) } $ cylinder o { height = pure 2, radius = pure 0.2 }

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

world1 :: IO ()
world1 = run scene2
