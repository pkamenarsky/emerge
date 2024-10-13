{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Event where

import Control.Monad (when)
import Control.Monad.IO.Class

import Data.Foldable
import Data.IORef
import qualified Data.Time.Clock.System as Time
import Data.Void
import qualified Data.Map as M
import qualified Data.Vector.Storable as V

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Network.HTTP.Client
-- import Network.HTTP.Client.MultipartFormData

import Types

import Syn

import GHC.Int
import GHC.Word

import qualified Sound.RtMidi as RT

--------------------------------------------------------------------------------

data EventFilter a where
  EFMouse :: GLFW.MouseButton -> GLFW.MouseButtonState -> EventFilter GLFW.ModifierKeys
  EFKey :: GLFW.Key -> GLFW.KeyState -> EventFilter GLFW.ModifierKeys

mouseDown :: GLFW.MouseButton -> EventFilter GLFW.ModifierKeys
mouseDown button = EFMouse button GLFW.MouseButtonState'Pressed

mouseUp :: GLFW.MouseButton -> EventFilter GLFW.ModifierKeys
mouseUp button = EFMouse button GLFW.MouseButtonState'Released

keyDown :: GLFW.Key -> EventFilter GLFW.ModifierKeys
keyDown key = EFKey key GLFW.KeyState'Pressed

keyUp :: GLFW.Key -> EventFilter GLFW.ModifierKeys
keyUp key = EFKey key GLFW.KeyState'Released

--------------------------------------------------------------------------------

data Input
  = InputMouse GLFW.MouseButton GLFW.MouseButtonState GLFW.ModifierKeys
  | InputKey GLFW.Key Int GLFW.KeyState GLFW.ModifierKeys

data EventContext = EventContext
  { ctxOn :: forall a v m. MonadIO m => EventFilter a -> Syn v m a
  }

data SignalContext = SignalContext
  { time :: Signal Float
  , ccRaw :: forall a. Num a => Word8 -> Signal (Maybe a)
  }

loop :: GLFW.Window -> (v -> IO ()) -> (SignalContext -> EventContext -> Syn [v] IO Void) -> IO ()
loop win render syn = do
  evtChan <- newIORef []

  ccMap <- newIORef mempty :: IO (IORef (M.Map Word8 Word8))
  dev <- RT.defaultInput

  RT.setCallback dev $ \_ msg -> do
   when (V.length msg >= 3) $ liftIO $ do
     putStrLn $ "id: " <> show (msg V.! 1) <> ", value: " <> show (msg V.! 2) <> ", ctrl: " <> show (msg V.! 0)
     atomicModifyIORef' ccMap (\m -> (M.insert (msg V.! 1) (msg V.! 2) m, ()))

  RT.openPort dev 1 "syn"

  GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled
  GLFW.setMouseButtonCallback win $ Just $ \_ button state modKeys -> modifyIORef' evtChan (InputMouse button state modKeys:)
  GLFW.setKeyCallback win $ Just $ \_ key scanCode state modKeys -> modifyIORef' evtChan (InputKey key scanCode state modKeys:)

  evtRef <- newIORef Nothing

  t0 <- Time.getSystemTime

  let ccSig = Signal $ fmap (\ccMap' ccId -> M.lookup ccId ccMap') (readIORef ccMap)
      sigCtx = SignalContext
        { time = Signal $ do
            now <- Time.getSystemTime

            let s = Time.systemSeconds now - Time.systemSeconds t0
            let ns = fi (Time.systemNanoseconds now) - fi (Time.systemNanoseconds t0) :: Int64

            pure $ fi s + fi ns / 1000000000
        , ccRaw = \ccId -> fmap (fmap fromIntegral) (ccSig <*> pure ccId)
        }

      evtCtx = EventContext
        { ctxOn = \evtFilter -> do
            mEvt <- unsafeNonBlockingIO $ readIORef evtRef

            case (mEvt, evtFilter) of
              (Just (InputMouse button state modKeys), EFMouse button' state')
                | button == button' && state == state' -> blocked $ pure modKeys

              (Just (InputKey key _ state modKeys), EFKey key' state')
                | key == key' && state == state' -> blocked $ pure modKeys

              -- no matching events
              _ -> blocked $ ctxOn evtCtx evtFilter
        }

      go mOut run = do
        running <- atomicModifyIORef evtChan nextEvent >>= \case
          Just (InputKey GLFW.Key'Escape _ GLFW.KeyState'Pressed _) -> pure False
          e -> do
            writeIORef evtRef e
            pure True

        when running $ do
          (next, rOut) <- unblock run

          -- [0]
          writeIORef evtRef Nothing
          (next', rOut') <- unblock next

          let mOut' = asum [rOut' >>= maybeHead, rOut >>= maybeHead, mOut]

          for_ mOut' render

          liftIO $ do
            GLFW.swapBuffers win
            GLFW.pollEvents

          go mOut' next'

  go Nothing $ reinterpret (syn sigCtx evtCtx)

  where
    fi :: (Integral a, Num b) => a -> b
    fi = fromIntegral

    nextEvent [] = ([], Nothing)
    nextEvent (evt:evts) = (evts, Just evt)

    maybeHead :: [a] -> Maybe a
    maybeHead (a:_) = Just a
    maybeHead _ = Nothing
