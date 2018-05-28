{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}

module Traycer.Graphics.RayTracing
  ( trace
  ) where

import Data.Foldable
import Data.Function
import Data.Maybe
import Control.Lens
import Linear.Epsilon
import Linear.Metric
import Linear.Vector
import Traycer.Math (epsilon, reflect)
import Traycer.Graphics.Color
import Traycer.Graphics.Body
import Traycer.Graphics.Light
import Traycer.Graphics.Texture
import Traycer.Geometry.Ray
import Traycer.Geometry.Solid
import Traycer.Config

-- | Checking whether ray collides with any body
collide :: (Epsilon a, Floating a, Ord a)
        => [Body a]
        -> Ray a
        -> Maybe (Collision a, Body a)
collide !bs !ray
  | null hitList = Nothing
  | otherwise    = Just $ minimumBy (compare `on` fst) unLifted
  where
    hitList = filter (isJust . fst) $ zip (map ((`hit` ray) . (^.solid)) bs) bs
    unLifted = map (\(Just x, y) -> (x, y)) hitList
{-# INLINE collide #-}

-- | The "engine" of the ray-tracer
--   This recursive function will trace a give ray
trace :: (Epsilon a, Floating a, Eq a, Ord a, Num b, Eq b)
      => Config a b          -- ^ Configuration of the scene
      -> Ray a               -- ^ Ray to trace
      -> Color a             -- ^ Color of ray traced
trace !config !ray
  | config^.depth == 0 = config^.ambient * sum (map (^.intensity) $ config^.lights)
  | otherwise          = case collide (config^.bodies) ray of
      Nothing     -> config ^.ambient
      Just (c, x) -> clip $ phongColor + reflColor + refrColor
        where
          config' = config&depth %~ subtract 1
          c' = c&scalar %~ subtract epsilon
          c'' = c&scalar %~ (+) epsilon
          phongColor = diffIllum config
                                 (x^.texture^.albedo)
                                 (x^.texture^.kAmbient)
                                 (x^.texture^.kDiffuse)
                                 (x^.texture^.kSpecular)
                                 (x^.texture^.specularExponent)
                                 (x^.solid)
                                 ray
                                 c'
          reflColor = reflIllum config'
                                (x^.texture^.reflectance)
                                (x^.solid)
                                ray
                                c'
          refrColor = refrIllum config'
                                (x^.texture^.transparency)
                                (x^.texture^.mu)
                                (x^.solid)
                                ray
                                c''
{-# INLINE trace #-}

-- ==========================
-- Temporary functions

diffIllum :: (Num a, Epsilon a, Floating a, Ord a, Num b, Eq b)
          => Config a b
          -> Color a        -- ^ Albedo
          -> a              -- ^ Ka
          -> a              -- ^ Kd
          -> a              -- ^ Ks
          -> a              -- ^ Specular Exponent
          -> Solid a
          -> Ray a
          -> Collision a 
          -> Color a
diffIllum !config !c !ka !kd !ks !e !x !ray !coll = ambientColor + diffColor + specColor
  where
    p = ray *-> (coll^.scalar)
    n = normalAt x ray coll
  
    -- | Rays from the point of intersection to light sources
    point2Lights = map (\l -> (l, p --> (l^.position) $ 1)) $ config^.lights
    nonShadowed = filter (isNothing . collide (config^.bodies) . snd) point2Lights
    intensities = map (\(l, lray) -> (l^.intensity, lray^.direction)) nonShadowed  

    -- | Color of a diffuse surface using phong model
    calcDiff (i, d) = c * i ^* dot n d
    calcSpec (i, d) = i ^* abs (dot (reflect d n) (ray^.direction)) ** e

    -- | Short circuit the multiplication for efficiency?
    ambientColor = ka !*! sum (map (^.intensity) $ config^.lights)
    diffColor = kd !*! sum (map calcDiff intensities)
    specColor = ks !*! sum (map calcSpec intensities)
{-# INLINE diffIllum #-}

reflIllum :: (Num a, Epsilon a, Floating a, Ord a, Num b, Eq b)
          => Config a b
          -> a          -- ^ Reflectance
          -> Solid a
          -> Ray a
          -> Collision a
          -> Color a
reflIllum !config !ref !x !ray !c = ref !*! reflColor
  where
    reflRay = reflected x ray c
    reflColor = trace config reflRay
{-# INLINE reflIllum #-}

refrIllum :: (Num a, Epsilon a, Floating a, Ord a, Num b, Eq b)
          => Config a b
          -> a            -- ^ Transparency
          -> a            -- ^ Refractive Index
          -> Solid a
          -> Ray a
          -> Collision a
          -> Color a
refrIllum !config !trs !m !x !ray !c = trs !*! refrColor 
  where
    refrRay = refracted x ray c m
    refrColor = fromMaybe 0 (trace config <$> refrRay)
{-# INLINE refrIllum #-}

-- ^ Short-circuit multiplication
(!*!) :: (Num a, Eq a, Functor b, Num (b a)) => a -> b a -> b a
(!*!) k v = if k == 0 then 0 else k *^ v
{-# INLINE (!*!) #-}

-- =========================
