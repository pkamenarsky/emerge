module Main (main) where

import Control.Exception
import Control.Monad.IO.Class

import Data.Foldable
import Data.Monoid
import Data.Void

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Render

import Syn
import Syn.Run

main :: IO ()
main = do
  _ <- GLFW.init

  GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 6
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core

  bracket
    (GLFW.createWindow 640 480 "SYN" Nothing Nothing)
    (traverse_ GLFW.destroyWindow)
    $ \mWin -> do
         let
             mouseSignal = Signal $ maybe (pure (0, 0)) GLFW.getCursorPos mWin
             fp = flip fmap mouseSignal $ \(mx, my) -> FillParams (GL.Color3 (tf $ mx / 640.0) (tf $ my / 480.0) 1.0) 

         GLFW.makeContextCurrent mWin
         rectBuf <- createRectBuffer
         (blitToScreen, _) <- blit rectBuf (GL.Size 640 480)

         for_ mWin (go blitToScreen Nothing $ reinterpret $ mapView (Last . Just) $ fillSyn rectBuf defaultOpOptions fp)

  putStrLn "bye..."

  where
    tf :: Double -> Float
    tf = realToFrac

    go :: (GL.TextureObject -> IO ()) -> Maybe Out -> Run (Last Out) IO Void -> GLFW.Window -> IO ()
    go blitToScreen mOut run win = do
      (next, rOut) <- unblock run

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
        else go blitToScreen mOut' next win
