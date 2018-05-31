{-# LANGUAGE BangPatterns #-}

module Traycer.Math
  ( quadratic
  , reflect
  , refract
  , rotationMatrix
  , epsilon
  ) where

import Linear.V3
import Linear.Matrix
import Linear.Vector
import Linear.Epsilon
import Linear.Metric

-- | Solve a quadratic equation and return it's solutions
quadratic :: (Floating a, Ord a, Eq a) => a -> a -> a -> Maybe (a, a)
quadratic !m !n !o
  | d < 0     = Nothing
  | d == 0    = Just (-0.5 * n / m, -0.5 * n / m)
  | otherwise = Just (min j k, max j k)
  where
    d = n * n - 4 * m * o
    q = if n > 0 then -0.5 * (n + sqrt d) else -0.5 * (n - sqrt d)
    j = q / m
    k = o / q
{-# INLINE quadratic #-}

-- | Calculate the reflection direction of a vector
--   against a normal vector
reflect :: (Epsilon a, Floating a) => V3 a -> V3 a -> V3 a
reflect !i !n = i - 2 * dot i n *^ n
{-# INLINE reflect #-}

-- | Calculate the refraction direction of a vector
--   against a normal vector given mu1 and mu2,
--   the refractive indices of the 2 media
refract :: (Epsilon a, Floating a, Ord a) => V3 a -> V3 a -> a -> a -> Maybe (V3 a)
refract !i !n !m1 !m2
  | sinsqtt > 1 = Nothing
  | otherwise   = Just $ m *^ i + (m * costi - sqrt (1 - sinsqtt)) *^ n
  where
    m = m1 / m2
    costi = -dot i n
    sinsqtt = m * m * (1 - costi * costi)
{-# INLINE refract #-}

-- | Construct the rotation matrix from angles in radian
rotationMatrix :: (Floating a) => a -> a -> a -> M33 a
rotationMatrix !x !y !z = rZ !*! rY !*! rX
  where
    rX = V3 (V3        1        0        0 )
            (V3        0  (cos x)  (-sin x))
            (V3        0  (sin x)   (cos x))
    rY = V3 (V3  (cos y)        0   (sin y))
            (V3        0        1        0 )
            (V3 (-sin y)        0  (cos y) )
    rZ = V3 (V3  (cos z) (-sin z)        0 )
            (V3  (sin z)  (cos z)        0 )
            (V3        0        0        1 )
{-# INLINE rotationMatrix #-}

-- | A small value close to zero
epsilon :: (Fractional a) => a
epsilon = 1e-6
{-# INLINE epsilon #-}
