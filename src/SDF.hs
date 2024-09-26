{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module SDF where

import           Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as ST
import qualified Control.Monad.Trans.Writer.CPS as W

import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T

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

newtype SDF = SDF { runSDF :: Pos -> W.WriterT [SDFDef] (ST.State Name) Pos }

genName :: W.WriterT [SDFDef] (ST.State Name) Name
genName = lift $ ST.state $ \(Name n) -> (Name n, Name (n + 1))

name :: Name -> Text
name (Name n) = "n_" <> T.pack (show n)

--------------------------------------------------------------------------------

data Field
data Value

type family P f v where
  P Field f = FieldDef f
  P Value v = v

--------------------------------------------------------------------------------

data BoxParams m = BoxParams
  { bpDimensions :: P m (GL.Vector3 Float)
  } deriving Generic

instance ShaderParam (BoxParams Value)
instance NamedShaderParam (BoxParams Field)

box :: Signal (BoxParams Value) -> SDF
box params = SDF $ \pos -> do
  prefix <- ("p_" <>) . name <$> genName
  out <- genName

  let opts = defaultShaderParamDeriveOpts
        { spFieldLabelModifier = (T.unpack prefix <>)
        }
      (uniforms, np) = namedShaderParam opts :: ([(Text, Text)], BoxParams Field)

  W.tell $ pure $ SDFDef
    { sdfIncludes = ["assets/lygia/sdf/boxSDF.glsl"]
    , sdfUniforms = uniforms
    , sdfDecls = [("float", out, [i| boxSDF(#{name pos}, #{field (bpDimensions np)} |])]
    , sdfSetParams = \program -> do
        set <- flip shaderParam program opts
        pure $ signalValue params >>= set
    }

  pure out

data TranslateParams m = TranslateParams
  { bpTranslate :: P m (GL.Vector3 Float)
  } deriving Generic

instance ShaderParam (TranslateParams Value)
instance NamedShaderParam (TranslateParams Field)

translate :: Signal (GL.Vector3 Float) -> SDF -> SDF
translate params sdf = SDF $ \pos -> do
  prefix <- ("p_" <>) . name <$> genName
  newPos <- genName

  let opts = defaultShaderParamDeriveOpts
        { spFieldLabelModifier = (T.unpack prefix <>)
        }
      (uniforms, np) = namedShaderParam opts :: ([(Text, Text)], TranslateParams Field)

      tParams :: GL.Vector3 Float -> TranslateParams Value
      tParams = TranslateParams

  W.tell $ pure $ SDFDef
    { sdfIncludes = []
    , sdfUniforms = uniforms
    , sdfDecls = [("vec3", newPos, [i| #{name pos} - #{field (bpTranslate np)} |])]
    , sdfSetParams = \program -> do
        set <- flip shaderParam program opts
        pure $ signalValue params >>= set . tParams
    }

  out <- runSDF sdf newPos

  pure out

compile :: SDF -> Text
compile sdf = compileDefs
  where
    pos0 = Name 0
    pos1 = Name 1

    (posn, defs) = flip ST.evalState pos1 $ W.runWriterT $ (runSDF sdf pos0)

    compileDefs = T.intercalate "\n" $ mconcat $
      [ [ [i|\#include "#{include}"|]
        | includes <- fmap sdfIncludes defs
        , include <- includes
        ]
      , []
      , [ [i|uniform #{ut} #{un};|]
        | uniforms <- fmap sdfUniforms defs
        , (ut, un) <- uniforms
        ]
      , []
      , [ [i|float sdf(vec3 #{name pos0}) { |] ]
      , [ [i|  #{dt} #{name dn} = #{dr};|]
        | decls <- fmap sdfDecls defs
        , (dt, dn, dr) <- decls
        ]
      , [ [i|  return #{name posn};|] ]
      , [ "}" ]
      ]
