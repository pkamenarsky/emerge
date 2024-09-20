module Main (main) where

import Control.Concurrent

import Data.Foldable

import Render

import qualified Graphics.UI.GLFW as GLFW

main :: IO ()
main = do
  _ <- GLFW.init

  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 6
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core

  mWin <- GLFW.createWindow 640 480 "SYN" Nothing Nothing

  GLFW.makeContextCurrent mWin

  render <- testrender

  for_ mWin (go render)

  putStrLn "bye..."

  where
    go render win = do
      render

      GLFW.swapBuffers win
      GLFW.waitEvents

      close <- GLFW.windowShouldClose win

      if close
        then do
          GLFW.destroyWindow win
          pure ()
        else go render win
