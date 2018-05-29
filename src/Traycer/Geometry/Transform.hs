{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE BangPatterns    #-}

module Traycer.Geometry.Transform
  ( Transform()
  , mkRotation
  , mkTranslation
  , rotation
  , rotationCenter
  , translate
  , transformSolid
  , transformVector
  ) where

import Control.Lens
import Linear.V3
import Linear.Matrix
import Linear.Epsilon
import GHC.Generics
import Traycer.Math
import Traycer.Geometry.Solid

data Transform a = Rotation { _rotation :: !(V3 a)
                            , _rotationCenter :: !(V3 a)
                            }
                 | Translate { _translate :: !(V3 a) }
                 deriving (Show, Read, Eq, Generic)

makeLenses ''Transform

mkRotation :: V3 a -> V3 a -> Transform a 
mkRotation = Rotation
{-# INLINE mkRotation #-}

mkTranslation :: V3 a -> Transform a 
mkTranslation = Translate
{-# INLINE mkTranslation #-}

-- | Apply a transform on the solid
transformSolid :: (Epsilon a, Floating a, Ord a) => Solid a -> Transform a -> Solid a
transformSolid !s (Translate !t) = changePoints s (+ t)
transformSolid !s (Rotation (V3 !x !y !z) !c) = final
  where
    translated = changePoints s (+ c)
    rotatedP = changePoints translated (rMatrix !*)
    rotated = changeDirections rotatedP (rMatrix !*)
    final = changePoints rotated (subtract c)
    rMatrix = rotationMatrix x y z
{-# INLINE transformSolid #-}

-- | Apply a transform to an arbitrary vector
transformVector :: (Floating a) => V3 a -> Transform a -> V3 a
transformVector !p (Translate !t) = p + t 
transformVector !p (Rotation (V3 !x !y !z) !c) = (rMatrix !* (p + c)) - c
  where
    rMatrix = rotationMatrix x y z
{-# INLINE transformVector #-}
