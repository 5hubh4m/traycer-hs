{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE BangPatterns    #-}

module Traycer.Config
  ( Config
  , mkConfig
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
import Traycer.Graphics.Body
import Traycer.Graphics.Light
import Traycer.Graphics.Camera
import Traycer.Graphics.Color

data Config a b = Config { _bodies :: ![Body a]     -- ^ 'Bodies' in the scene
                         , _lights :: ![Light a]    -- ^ 'Light's in the scene
                         , _ambient :: !(Color a)   -- ^ Ambient light's 'Color' in the scene
                         , _camera :: !(Camera a b) -- ^ Location and size of 'Camera'
                         , _depth :: !b             -- ^ Depth of tracing
                         , _aaSamples :: !b         -- ^ Anti-alisasing supersampling amount
                         , _dofSamples :: !b        -- ^ Depth of field samplinfg amount
                         }
                deriving (Show, Read, Generic)

makeLenses ''Config

mkConfig :: (Num b, Ord b)
         => [Body a]
         -> [Light a]
         -> Color a
         -> Camera a b
         -> b
         -> b
         -> b
         -> Config a b
mkConfig !bs !ls !a !c !d !aa !dof
  | aa < 1    = error "Invalid number of Anti-aliasing samples."
  | dof < 1   = error "Invalid number of Depth of field samples."
  | otherwise = Config bs ls a c d aa dof
