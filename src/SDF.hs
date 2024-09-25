{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SDF where

import Data.Text (Text)
import qualified Data.Text as T
import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as ST
import qualified Control.Monad.Trans.Writer.CPS as W

import qualified Graphics.Rendering.OpenGL as GL

import GHC.Generics (Generic)

import Syn.Run
import Types

--------------------------------------------------------------------------------

newtype Name = Name Int
type Pos = Name

data SDFDef = SDFDef
  { sdfIncludes  :: [Text]
  , sdfUniforms  :: [(Text, Text)]
  , sdfDecls     :: [(Text, Name, Text)]
  , sdfSetParams :: GL.Program -> IO (IO ())
  }

newtype SDF = SDF { runSDF :: Pos -> W.WriterT [SDFDef] (ST.State Name) Name }

genName :: W.WriterT [SDFDef] (ST.State Name) Name
genName = lift $ ST.state $ \(Name n) -> (Name n, Name (n + 1))

name :: Name -> T.Text
name (Name n) = T.pack (show n)

--------------------------------------------------------------------------------

data BoxParams = BoxParams
  { bpDimensions :: GL.Vertex3 Float
  } deriving (Generic, ShaderParam)

box :: Signal BoxParams -> SDF
box params = SDF $ \pos -> do
  prefix <- ("p_" <>) . name <$> genName
  out <- genName

  W.tell $ pure $ SDFDef
    { sdfIncludes = ["assets/lygia/sdf/boxSDF.glsl"]
    , sdfUniforms = [("vec3", prefix <> "bpDimensions)")]
    , sdfDecls = [("float", out, "boxSDF(" <> name pos <> ", " <> prefix <> "bpDimensions)")]
    , sdfSetParams = \program -> do
        set <- flip shaderParam program $ defaultShaderParamDeriveOpts
          { spFieldLabelModifier = (T.unpack prefix <>)
          }
        pure $ signalValue params >>= set
    }

  pure out

translate :: Signal (GL.Vector3 Float) -> SDF -> SDF
translate params sdf = SDF $ \pos -> do
  newPos <- genName

  W.tell $ pure $ SDFDef
    { sdfDecls = [("vec3", newPos, undefined)]
    }

  out <- runSDF sdf newPos

  pure out
