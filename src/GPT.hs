{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module GPT where

import Control.Monad.IO.Class
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as ST
import qualified Control.Monad.Trans.Writer.CPS as W

import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T

import qualified Graphics.Rendering.OpenGL as GL

import GHC.Generics (Generic)

import Render
import Syn
import Syn.Run
import Types

data DefaultParams m = DefaultParams
  { u_resolution :: P m (GL.Vector2 Float)
  , u_time :: P m Float
  } deriving Generic

instance ShaderParam (DefaultParams Values)
instance NamedShaderParam DefaultParams

gptShader :: RectBuffer -> OpOptions -> IO (Op (DefaultParams Values))
gptShader = undefined

gptShaderSyn :: MonadIO m => RectBuffer -> OpOptions -> Signal (DefaultParams Values) -> Syn [Out] m a
gptShaderSyn rectBuf opts params = do
  Op tex render destroy <- unsafeNonBlockingIO $ gptShader rectBuf opts

  finalize (liftIO destroy) $ view $ pure $ Out
    { outTex = tex
    , outRender = signalValue params >>= render
    }
