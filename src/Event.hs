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
  , ccRaw :: forall a. Num a => Word8 -> Signal a
  , mousePos :: Signal (Float, Float)
  }

toRange :: Float -> Float -> Word8 -> Float
toRange mi ma w = mi + (realToFrac w / 127.0 * (ma - mi))

cc :: SignalContext -> Word8 -> Float -> Float -> Signal Float
cc ctx ccId i a = fmap (toRange i a) (ccRaw ctx ccId)

signalContext :: GLFW.Window -> IO SignalContext
signalContext win = do
  -- MIDI
  ccMap <- newIORef M.empty

  dev <- liftIO $ RT.defaultInput

  RT.setCallback dev $ \_ msg -> do
    when (V.length msg >= 3) $ liftIO $ do
      putStrLn $ "id: " <> show (msg V.! 1) <> ", value: " <> show (msg V.! 2) <> ", ctrl: " <> show (msg V.! 0)
      atomicModifyIORef' ccMap (\m -> (M.insert (msg V.! 1) (msg V.! 2) m, ()))

  RT.openPort dev 1 "syn"

  --

  t0 <- Time.getSystemTime

  pure $ SignalContext
    { time = Signal $ do
        now <- Time.getSystemTime

        let s = Time.systemSeconds now - Time.systemSeconds t0
        let ns = fi (Time.systemNanoseconds now) - fi (Time.systemNanoseconds t0) :: Int64

        pure $ Just $ fi s + fi ns / 1000000000

    , ccRaw = \ccId -> Signal $ readIORef ccMap >>= \m -> pure $ fmap fi $ M.lookup ccId m
    , mousePos = Signal $ fmap (Just . tf) $ GLFW.getCursorPos win 
    }
  where
    fi :: (Integral a, Num b) => a -> b
    fi = fromIntegral

    tf (a, b) = (realToFrac a, realToFrac b)

eventContext :: IO (IORef (Maybe Input), EventContext)
eventContext = do
  evtRef <- newIORef Nothing

  let evtCtx = EventContext
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

  pure (evtRef, evtCtx)

loop :: MonadIO m => GLFW.Window -> IORef (Maybe Input) -> (v -> IO ()) -> Syn [v] m Void -> m ()
loop win evtRef render syn = do
  evtChan <- liftIO $ newIORef []

  liftIO $ do
    GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled
    GLFW.setMouseButtonCallback win $ Just $ \_ button state modKeys -> modifyIORef' evtChan (InputMouse button state modKeys:)
    GLFW.setKeyCallback win $ Just $ \_ key scanCode state modKeys -> modifyIORef' evtChan (InputKey key scanCode state modKeys:)

  let go mOut run = do
        running <- liftIO $ atomicModifyIORef evtChan nextEvent >>= \case
          Just (InputKey GLFW.Key'Escape _ GLFW.KeyState'Pressed _) -> pure False
          e -> do
            writeIORef evtRef e
            pure True

        when running $ do
          (next, rOut) <- unblock run

          -- [0]
          liftIO $ writeIORef evtRef Nothing
          (next', rOut') <- unblock next

          let mOut' = asum [rOut' >>= maybeHead, rOut >>= maybeHead, mOut]

          liftIO $ do
            for_ mOut' render

            GLFW.swapBuffers win
            GLFW.pollEvents

          go mOut' next'

  go Nothing $ reinterpret syn

  where
    nextEvent [] = ([], Nothing)
    nextEvent (evt:evts) = (evts, Just evt)

    maybeHead :: [a] -> Maybe a
    maybeHead (a:_) = Just a
    maybeHead _ = Nothing
