{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE BangPatterns    #-}

module Traycer.Config
  ( Config
  , SolidRef
  , mkConfig
  , mkConfigFromTextures
  , bodies
  , lights
  , ambient
  , camera
  , depth
  , aaSamples
  , dofSamples
  ) where

import Control.Lens
import GHC.Generics
import Traycer.Geometry.Solid
import Traycer.Graphics.Body hiding (solid, texture)
import Traycer.Graphics.Light
import Traycer.Graphics.Camera
import Traycer.Graphics.Color
import Traycer.Graphics.Texture

data Config a b = Config { _bodies :: ![Body a]     -- ^ 'Bodies' in the scene
                         , _lights :: ![Light a]    -- ^ 'Light's in the scene
                         , _ambient :: !(Color a)   -- ^ Ambient light's 'Color' in the scene
                         , _camera :: !(Camera a b) -- ^ Location and size of 'Camera'
                         , _depth :: !b             -- ^ Depth of tracing
                         , _aaSamples :: !b         -- ^ Anti-alisasing supersampling amount
                         , _dofSamples :: !b        -- ^ Depth of field samplinfg amount
                         }
                deriving (Show, Read, Generic)

data SolidRef a b = SolidRef { _solid :: !(Solid a) -- ^ 'Solid'
                             , _texture :: !b       -- ^ Index in the 'Texture's list.
                             }
                  deriving (Show, Read, Generic)

makeLenses ''Config
makeLenses ''SolidRef

mkConfig :: (Num b, Ord b, Integral b)
         => [Body a]
         -> [Light a]
         -> Color a
         -> Camera a b
         -> b
         -> b
         -> b
         -> Config a b
mkConfig !bs !ls !a !c !d !aa !dof
  | aa < 1                 = error "Invalid number of Anti-aliasing samples."
  | dof < 1                = error "Invalid number of Depth of field samples."
  | otherwise              = Config bs ls a c d aa dof
{-# INLINE mkConfig #-}

mkConfigFromTextures :: (Num b, Ord b, Integral b)
                     => [Texture a]
                     -> [SolidRef a b]
                     -> [Light a]
                     -> Color a
                     -> Camera a b
                     -> b
                     -> b
                     -> b
                     -> Config a b
mkConfigFromTextures !txs !srs !ls !a !c !d !aa !dof
  | aa < 1                 = error "Invalid number of Anti-aliasing samples."
  | dof < 1                = error "Invalid number of Depth of field samples."
  | maxIndex > texListSize = error "Texture index greater than bound."
  | minIndex < 0           = error "Texture index less than 0."
  | otherwise              = Config sRefs2Bodies ls a c d aa dof
  where
    indexList = map (^.texture) srs
    minIndex = minimum indexList
    maxIndex = maximum indexList
    texListSize = fromIntegral (length txs) - 1
    sRefs2Bodies = map (\sref -> Body (sref^.solid) (txs !! fromIntegral (sref^.texture))) srs
{-# INLINE mkConfigFromTextures #-}
