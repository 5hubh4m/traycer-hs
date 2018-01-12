{-# LANGUAGE TemplateHaskell #-}

module Traycer.Graphics.Light
  ( Light(..)
  , intensity
  , position
  ) where

import Control.Lens
import Linear.V3
import Traycer.Graphics.Color

data Light a = Light { _intensity :: !(Color a)
                     , _position :: !(V3 a)
                     }
             deriving (Show, Eq)
  
makeLenses ''Light
