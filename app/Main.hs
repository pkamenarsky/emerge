module Main (main) where

import Control.Exception
import Control.Monad (void)
import Control.Monad.IO.Class

import Data.Foldable
import Data.IORef
import Data.StateVar
import Data.Void

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Render
import SDF

import Syn
import Syn.Run

import Debug.Trace (traceIO)

scene :: MonadIO m => RectBuffer -> Event () -> Signal (Double, Double) -> Syn [Out] m a
scene rectBuf mouseClick mousePos = do
  -- asum [ void $ circleSyn rectBuf defaultOpOptions (circleParams 0.05), on mouseClick ]
  -- asum [ void $ circleSyn rectBuf defaultOpOptions (circleParams 0.1), on mouseClick ]
  -- asum [ void $ circleSyn rectBuf defaultOpOptions (circleParams 0.15), on mouseClick ]

  sdfSyn rectBuf defaultOpOptions
    (trace (pure $ defaultTraceParams { tpMaxIterations = 10, tpFresnelBase = 1, tpFresnelExp = 2 }))
    (rotate rotateY $ rotate rotateX $ box (pure $ BoxParams (GL.Vector3 0.5 0.5 0.3)))
  -- blendSyn rectBuf defaultBlendOptions (pure $ BlendParamsSyn 0.5)
  --   (fillSyn rectBuf defaultOpOptions fillParams)
  --   (circleSyn rectBuf defaultOpOptions (circleParams 0.15))

  where
    rotateX :: Signal (RotateParams Value)
    rotateX = fmap (\(x, _) -> RotateParams (GL.Vector3 0 1 0) (tf $ x / (-100))) mousePos

    rotateY :: Signal (RotateParams Value)
    rotateY = fmap (\(_, y) -> RotateParams (GL.Vector3 1 0 0) (tf $ y / 100)) mousePos

    tf = realToFrac

    fillParams = flip fmap mousePos $ \(mx, my) -> FillParams
      (GL.Color4 (tf $ mx / 1024.0) (tf $ my / 1024.0) 1 1) 

    circleParams radius = flip fmap mousePos $ \(mx, my) -> CircleParams
      (GL.Color4 (tf $ mx / 1024.0) (tf $ my / 1024.0) 1 1) 
      (GL.Vertex2 (tf $ mx / 1024.0) (1 - tf (my / 1024.0)))
      radius

main :: IO ()
main = do
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

         for_ mWin (go False mouseClick blitToScreen Nothing $ reinterpret $ scene rectBuf mouseClick mousePos)

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
          GLFW.waitEvents
          go mouseButtonSt' e blitToScreen mOut' next' win
