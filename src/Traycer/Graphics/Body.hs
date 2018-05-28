{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Traycer.Graphics.Body
  ( Body()
  , solid
  , texture
  , mkBody
  ) where

import GHC.Generics
import Linear.Epsilon
import Traycer.Geometry.Solid
import Traycer.Geometry.Transform
import Traycer.Graphics.Texture
import Control.Lens

data Body a = Body { _solid :: !(Solid a)
                   , _texture :: !(Texture a)
                   }
            deriving (Show, Read, Eq, Generic)
          
makeLenses ''Body

-- | Make body with transforms
mkBody :: (Floating a, Epsilon a) => Solid a -> Texture a -> [Transform a] -> Body a
mkBody s t ts = Body (foldl applyTransform s ts) t
{-# INLINE mkBody #-}
