{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module Traycer.Graphics.Body
  ( Body(..)
  , solid
  , texture
  ) where

import Traycer.Geometry.Solid
import Traycer.Graphics.Texture
import Control.Lens

data Body a = Body { _solid :: !(Solid a)
                   , _texture :: !(Texture a)
                   }
            deriving (Show, Eq)
          
makeLenses ''Body
