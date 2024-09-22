module Main (main) where

import Control.Exception

import Data.Foldable

import qualified Graphics.UI.GLFW as GLFW

import Render

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
         GLFW.makeContextCurrent mWin
         render <- testrender2
         for_ mWin (go render)

  putStrLn "bye..."

  where
    go :: IO () -> GLFW.Window -> IO ()
    go render win = do
      render

      GLFW.swapBuffers win
      GLFW.waitEvents

      esc <- GLFW.getKey win GLFW.Key'Escape

      close <- GLFW.windowShouldClose win

      if close || esc == GLFW.KeyState'Pressed
        then pure ()
        else go render win
