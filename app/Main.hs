module Main (main) where

import Control.Exception
import Control.Monad (void)
import Control.Monad.IO.Class

import Data.Foldable
import Data.IORef
import Data.Monoid
import Data.Void

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Render

import Syn
import Syn.Run

scene :: MonadIO m => RectBuffer -> Event () -> Signal (Double, Double) -> Syn Out m a
scene rectBuf mouseClick mousePos = do
  asum [ void $ circleSyn rectBuf defaultOpOptions (circleParams 0.05), on mouseClick ]
  asum [ void $ circleSyn rectBuf defaultOpOptions (circleParams 0.1), on mouseClick ]
  asum [ void $ circleSyn rectBuf defaultOpOptions (circleParams 0.15), on mouseClick ]

  fillSyn rectBuf defaultOpOptions fillParams

  where
    tf = realToFrac

    fillParams = flip fmap mousePos $ \(mx, my) -> FillParams
      (GL.Color4 (tf $ mx / 1024.0) (tf $ my / 1024.0) 1 1) 

    circleParams radius = flip fmap mousePos $ \(mx, my) -> CircleParams
      (GL.Color4 (tf $ mx / 1024.0) (tf $ my / 1024.0) 1 1) 
      (GL.Vertex2 (tf $ mx / 1024.0) (1 - tf (my / 1024.0)))
      radius
      0.01

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
         rectBuf <- createRectBuffer
         (blitToScreen, _) <- blit rectBuf (GL.Size 1024 1024)

         for_ mWin (go mouseClick blitToScreen Nothing $ reinterpret $ mapView (Last . Just) $ scene rectBuf mouseClick mousePos)

  putStrLn "bye..."

  where
    tf :: Double -> Float
    tf = realToFrac

    go :: Event () -> (GL.TextureObject -> IO ()) -> Maybe Out -> Run (Last Out) IO Void -> GLFW.Window -> IO ()
    go e@(Event mouseClick) blitToScreen mOut run win = do
      st <- GLFW.getMouseButton win GLFW.MouseButton'1

      case st of
        GLFW.MouseButtonState'Pressed -> writeIORef mouseClick (Just ())
        _ -> pure ()

      (next, rOut) <- unblock run

      writeIORef mouseClick Nothing

      let mOut' = asum [rOut >>= getLast, mOut]

      for_ mOut' $ \out -> do
         outRender out
         blitToScreen (outTex out)

      GLFW.swapBuffers win
      GLFW.waitEvents

      esc <- GLFW.getKey win GLFW.Key'Escape

      close <- GLFW.windowShouldClose win

      if close || esc == GLFW.KeyState'Pressed
        then pure ()
        else go e blitToScreen mOut' next win
