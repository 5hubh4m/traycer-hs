{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Traycer.Geometry.Solid
  ( Solid
  , mkDisk
  , mkPlane
  , mkSphere
  , center
  , normal
  , radius
  , isIn
  , hit
  , normalAt
  , reflected
  , refracted
  ) where

import Control.Lens
import GHC.Generics
import Linear.Epsilon
import Linear.Metric
import Linear.V3
import Traycer.Geometry.Ray
import Traycer.Math.Misc

data Solid a = Plane { _center :: !(V3 a)  -- ^ Any point that passes through the plane
                     , _normal :: !(V3 a) -- ^ The normal `vector` should always be normalised
                     }
             | Sphere { _center :: !(V3 a)
                      , _radius :: !a
                      }
             | Disk { _center :: !(V3 a) -- ^ The center of the disk
                    , _normal :: !(V3 a) -- ^ The normal `vector` should always be normalised
                    , _radius :: !a      -- ^ Radius of the disk
                    }
             deriving (Show, Read, Eq, Generic)

makeLenses ''Solid

-- Contructors
mkSphere :: (Num a, Ord a) => V3 a -> a -> Solid a
mkSphere c r
  | r < 0     = error "Negative radius for sphere."
  | otherwise = Sphere c r
{-# INLINE mkSphere #-}

mkPlane :: (Floating a, Epsilon a) => V3 a -> V3 a -> Solid a
mkPlane c n = Plane c $ normalize n
{-# INLINE mkPlane #-}

mkDisk :: (Floating a, Epsilon a, Ord a) => V3 a -> V3 a -> a -> Solid a
mkDisk c n r
  | r < 0     = error "Negative radius for disk."
  | otherwise = Disk c (normalize n) r
{-# INLINE mkDisk #-}

-- | Solid type class,
--   implements helper functions for
--   collision etcetr
-- | Check whether point lies inside solid
isIn :: (Epsilon a, Floating a, Ord a, Eq a)
     => Solid a
     -> V3 a
     -> Bool
isIn Plane{} _ = False
isIn Disk{} _ = False
isIn (Sphere c r) p = norm (p - c) < r
{-# INLINE isIn #-}

-- | Test for collision with a solid
--   If collision is successful, return the ray
--   coordinate for the point of intersection
hit :: (Epsilon a, Floating a, Ord a, Eq a)
    => Solid a                    -- ^ The 'Solid' to test collision with
    -> Ray a                      -- ^ 'Ray' to hit
    -> Maybe a                    -- ^ Scalar which gives the intersection point
hit (Plane c n) ray
  | nearZero denom || t <= 0 = Nothing
  | otherwise                = Just t
  where
    (denom, t) = hitFlatSurface c n ray
hit (Disk c n r) ray
  | nearZero denom || t <= 0 || (norm (c - p) > r) = Nothing
  | otherwise                                      = Just t
  where
    (denom, t) = hitFlatSurface c n ray
    p = ray *-> t
hit (Sphere o r) ray = case quadratic a b c of
  Nothing -> Nothing
  Just (t0, t1)
    | t0 <= 0 && t1 <= 0 -> Nothing
    | t0 <= 0 && t1 > 0  -> Just t1
    | otherwise          -> Just $ min t0 t1
  where
    l = ray^.origin - o
    a = quadrance $ ray^.direction
    b = 2 * dot (ray^.direction) l
    c = quadrance l - r * r
{-# INLINE hit #-}

-- | Get the normal at the point of intersection
normalAt :: (Epsilon a, Floating a, Ord a, Eq a)
       => Solid a
       -> Ray a
       -> a
       -> V3 a                    -- ^ Normal vector at the intersection point, normalised
normalAt (Plane c n) ray _ = if dot n (ray^.origin - c) >= 0
                             then n
                             else -n
normalAt (Disk c n _) ray _ = if dot n (ray^.origin - c) >= 0
                              then n
                              else -n
normalAt s@(Sphere c _) ray t = normalize $ if isIn s $ ray^.origin
                                            then c - p
                                            else p - c
  where
    p = ray *-> t
{-# INLINE normalAt #-}

-- | Get the reflected ray from the intersection
--   of ray with solid
reflected :: (Epsilon a, Floating a, Ord a, Eq a)
          => Solid a
          -> Ray a
          -> a
          -> Ray a                -- ^ Reflected ray at intersection point
reflected s r t = mkRay p (reflect (r^.direction) n) (r^.medium)
  where
    p = r *-> t
    n = normalAt s r t
{-# INLINE reflected #-}

-- | Get the refracted ray from the intersection
--  of ray with the solid
refracted :: (Epsilon a, Floating a, Ord a, Eq a)
          => Solid a
          -> Ray a
          -> a
          -> (a -> Maybe (Ray a)) -- | returns a function that takes
                                  --   mu and maybe returns a refracted ray
refracted Plane{} r _ _ = Just r
refracted Disk{} r _ _ = Just r
refracted s r t mu = do
  refr <- refract (r^.direction) n (r^.medium) mu
  return $ mkRay p refr mu
    where
      p = r *-> t
      n = normalAt s r t 
{-# INLINE refracted #-}

-- ======================
-- Temporary functions

hitFlatSurface :: (Num a, Fractional a) => V3 a -> V3 a -> Ray a -> (a, a)
hitFlatSurface c n ray = (denom, t)
  where
    denom = dot (ray^.direction) n
    numer = dot (c - ray^.origin) n
    t = numer / denom
{-# INLINE hitFlatSurface #-}

-- ======================
