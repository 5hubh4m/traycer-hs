{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Traycer.Graphics.Body
  ( Body(..)
  , solid
  , texture
  ) where

import GHC.Generics
import Traycer.Geometry.Solid
import Traycer.Graphics.Texture
import Control.Lens

data Body a = Body { _solid :: !(Solid a)
                   , _texture :: !(Texture a)
                   }
            deriving (Show, Read, Eq, Generic)
          
makeLenses ''Body
