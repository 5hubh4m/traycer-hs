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

-- | Dummy instance for Ord (Body a)
instance (Eq a) => Ord (Body a) where
  _ <= _ = False

-- | Make body with transforms
mkBody :: (Floating a, Epsilon a, Ord a) => Solid a -> Texture a -> [Transform a] -> Body a
mkBody s t ts = Body (foldl transformSolid s ts) t
{-# INLINE mkBody #-}
