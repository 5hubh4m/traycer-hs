{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}

module Traycer.Config
  ( Config(..)
  , bodies
  , lights
  , ambient
  , camera
  , depth
  ) where

import Control.Lens
import Traycer.Graphics.Body
import Traycer.Graphics.Light
import Traycer.Graphics.Camera
import Traycer.Graphics.Color

data Config a b = Config { _bodies :: ![Body a]     -- ^ 'Bodies' in the scene
                         , _lights :: ![Light a]    -- ^ 'Light's in the scene
                         , _ambient :: !(Color a)   -- ^ Ambient light's 'Color' in the scene
                         , _camera :: !(Camera a b) -- ^ Location and size of 'Camera'
                         , _depth :: !b             -- ^ Depth of tracing
                         }
                deriving (Show)

makeLenses ''Config
