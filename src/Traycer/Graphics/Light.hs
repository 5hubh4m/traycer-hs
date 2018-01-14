{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Traycer.Graphics.Light
  ( Light(..)
  , intensity
  , position
  ) where

import GHC.Generics
import Control.Lens
import Linear.V3
import Traycer.Graphics.Color

data Light a = Light { _intensity :: !(Color a)
                     , _position :: !(V3 a)
                     }
             deriving (Show, Read, Eq, Generic)
  
makeLenses ''Light
