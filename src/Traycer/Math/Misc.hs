{-# LANGUAGE BangPatterns #-}

module Traycer.Math.Misc
  ( quadratic
  , reflect
  , refract
  , reflectance
  , refractance
  , fresnel
  , epsilon
  ) where

import Linear.V3
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
reflect !j !k = i - 2 * dot i n *^ n
  where
    i = normalize j
    n = normalize k
{-# INLINE reflect #-}

-- | Calculate the refraction direction of a vector
--   against a normal vector given mu1 and mu2,
--   the refractive inices of the 2 media
refract :: (Epsilon a, Floating a, Ord a) => V3 a -> V3 a -> a -> a -> Maybe (V3 a)
refract !j !k !m1 !m2
  | sinsqtt > 1 = Nothing
  | otherwise   = Just $ normalize $ m *^ i + (m * costi - sqrt (1 - sinsqtt)) *^ n
  where
    (i, n, m, costi, sinsqtt) = goodies j k m1 m2
{-# INLINE refract #-}

reflectance :: (Epsilon a, Floating a, Ord a) => V3 a -> V3 a -> a -> a -> a
reflectance !j !k !m1 !m2
  | sinsqtt > 1 = 1
  | m1 > m2 && sinsqtt <= 1 = r0 + (1 - r0) * x * x * x * x * x
  | otherwise               = r0 + (1 - r0) * y * y * y * y * y
  where
    (_, _, _, costi, sinsqtt) = goodies j k m1 m2
    r0 = ((m1 - m2) / (m1 + m2)) * ((m1 - m2) / (m1 + m2))
    x = 1 - sqrt (1 - sinsqtt)
    y = 1 - costi
{-# INLINE reflectance #-}
    
refractance :: (Num a, Epsilon a, Floating a, Ord a) => V3 a -> V3 a -> a -> a -> a
refractance !j !k !m1 !m2 = 1 - reflectance j k m1 m2
{-# INLINE refractance #-}

fresnel :: (Num a, Epsilon a, Floating a, Ord a) => V3 a -> V3 a -> a -> a -> (a, a)
fresnel !j !k !m1 !m2 = (reflectance j k m1 m2, refractance j k m1 m2)
{-# INLINE fresnel #-}

-- | A small value close to zero
epsilon :: (Fractional a) => a
epsilon = 1e-6
{-# INLINE epsilon #-}

-- ======================
-- Temporary functions

goodies :: (Epsilon a, Floating a, Ord a)
        => V3 a -> V3 a -> a -> a
        -> (V3 a, V3 a, a, a, a)
goodies !j !k !m1 !m2 = (i, n, m, costi, sinsqtt)
  where
    i = normalize j
    n = normalize k
    m = m1 / m2
    costi = -dot i n
    sinsqtt = m * m * (1 - costi * costi)
{-# INLINE goodies #-}

-- ======================
