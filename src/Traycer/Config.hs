{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE BangPatterns    #-}

module Traycer.Config
  ( Config
  , SolidRef(..)
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
import Linear.Epsilon
import GHC.Generics
import Traycer.Geometry.Solid
import Traycer.Geometry.Transform
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
                             , _texture :: !b       -- ^ Index in the 'Texture's list
                             , _transforms :: ![Transform a]
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

mkConfigFromTextures :: (Floating a, Epsilon a, Ord a, Num b, Ord b, Integral b)
                     => [Texture a]
                     -> [SolidRef a b]
                     -> [Light a]
                     -> Color a
                     -> Camera a b
                     -> b
                     -> b
                     -> b
                     -> [Transform a]
                     -> Config a b
mkConfigFromTextures !txs !srs !ls !a !c !d !aa !dof !ts
  | aa < 1                 = error "Invalid number of Anti-aliasing samples."
  | dof < 1                = error "Invalid number of Depth of field samples."
  | maxIndex > texListSize = error "Texture index greater than bound."
  | minIndex < 0           = error "Texture index less than 0."
  | otherwise              = Config bs ls' a c d aa dof
  where
    indexList = map (^.texture) srs
    ls' = map (&position %~ (\p -> foldl transformVector p ts)) ls
    minIndex = minimum indexList
    maxIndex = maximum indexList
    texListSize = fromIntegral (length txs) - 1
    bs = map idxBody srs
    idxBody s = mkBody (s^.solid) (txs !! fromIntegral (s^.texture)) $ (s^.transforms) ++ ts
{-# INLINE mkConfigFromTextures #-}
