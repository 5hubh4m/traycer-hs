{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE BangPatterns    #-}

module Traycer.Geometry.Transform
  ( Transform()
  , mkRotation
  , mkTranslation
  , rotationX
  , rotationY
  , rotationZ
  , rotationCenter
  , translate
  , applyTransform
  ) where

import Control.Lens
import Linear.V3
import Linear.Matrix
import Linear.Epsilon
import GHC.Generics
import Traycer.Math
import Traycer.Geometry.Solid

data Transform a = Rotation { _rotationX :: !a
                            , _rotationY :: !a
                            , _rotationZ :: !a
                            , _rotationCenter :: !(V3 a)
                            }
                 | Translate { _translate :: !(V3 a) }
                 deriving (Show, Read, Eq, Generic)

makeLenses ''Transform

mkRotation :: a -> a -> a -> V3 a -> Transform a 
mkRotation = Rotation
{-# INLINE mkRotation #-}

mkTranslation :: V3 a -> Transform a 
mkTranslation = Translate
{-# INLINE mkTranslation #-}

-- | Apply a transform on the solid
applyTransform :: (Epsilon a, Floating a) => Solid a -> Transform a -> Solid a
applyTransform !s (Translate !t) = changePoints s (+ t)
applyTransform !s (Rotation !x !y !z !c) = final
  where
    translated = changePoints s (+ c)
    rotatedP = changePoints translated (rMatrix !*)
    rotated = changeDirections rotatedP (rMatrix !*)
    final = changePoints rotated (subtract c)
    rMatrix = rotationMatrix x y z
{-# INLINE applyTransform #-}
