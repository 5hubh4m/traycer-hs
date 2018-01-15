{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns    #-}

module Traycer.Geometry.Ray
  ( Ray
  , mkRay
  , origin
  , direction
  , medium
  , (*->)
  , (-->)
  ) where

import Control.Lens
import Linear.V3
import Linear.Vector
import Linear.Metric
import Linear.Epsilon

data Ray a = Ray { _origin :: !(V3 a)    -- ^ Origin of the ray
                 , _direction :: !(V3 a) -- ^ Direction of the ray, must be normalised
                 , _medium :: !a         -- ^ Refractive index of the medium
                 }
           deriving (Show, Eq)

makeLenses ''Ray

-- | Make a new 'Ray' by normalising the direction vector
mkRay :: (Epsilon a, Floating a) => V3 a -> V3 a -> a -> Ray a 
mkRay !o !d !m = Ray { _origin = o
                     , _direction = normalize d
                     , _medium = m
                     }
{-# INLINE mkRay #-}

-- | Make a ray going from p1 to p2
(-->) :: (Epsilon a, Floating a) => V3 a -> V3 a -> a -> Ray a
(-->) !p1 !p2 = mkRay p1 (p2 - p1) 
{-# INLINE (-->) #-}

-- | Translate the ray 'r's origin by a scalar 't'
(*->) :: (Num a) => Ray a -> a -> V3 a
(*->) !r !t = (r^.origin) + t *^ (r^.direction)
{-# INLINE (*->) #-}
