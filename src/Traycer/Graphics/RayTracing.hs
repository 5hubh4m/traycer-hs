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
        -> Maybe (a, Body a)
collide !bs !ray
  | null ts   = Nothing
  | otherwise = Just $ minElem&_1 %~ fromMaybe 0
  where
    ts = filter (isJust . fst) $ zip (map ((`hit` ray) . (^.solid)) bs) bs
    minElem = minimumBy (compare `on` fst) ts
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
      Just (t, x) -> clip $ phongColor + reflColor + refrColor
        where
          config' = config&depth %~ subtract 1
          phongColor = diffIllum
                       config
                       (x^.texture^.albedo)
                       (x^.texture^.kAmbient)
                       (x^.texture^.kDiffuse)
                       (x^.texture^.kSpecular)
                       (x^.texture^.specularExponent)
                       (x^.solid)
                       ray
                       t
          reflColor = reflIllum
                      config'
                      (x^.texture^.reflectance)
                      (x^.solid)
                      ray
                      t
          refrColor = refrIllum
                      config'
                      (x^.texture^.transparency)
                      (x^.texture^.mu)
                      (x^.solid)
                      ray
                      t
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
          -> a 
          -> Color a
diffIllum !config !c !ka !kd !ks !e !x !ray !t = clip $ ambientColor + diffColor + specColor
  where
    p = ray *-> (t - epsilon)
    n = normalAt x ray t
  
    -- | Rays from the point of intersection to light sources
    point2Lights = map (\l -> (l, p --> (l^.position) $ 1)) $ config^.lights
    nonShadowed = filter (isNothing . collide (config^.bodies) . snd) point2Lights
    intensities = map (\(l, lray) -> (l^.intensity, lray^.direction)) nonShadowed  

    -- | Color of a diffuse surface using phong model
    calcDiff (i, d) = c * i ^* dot n d
    calcSpec (i, d) = i ^* abs (dot (reflect d n) (ray^.direction)) ** e

    -- | Short circuit the multiplication for efficiency?
    ambientColor = if ka == 0
                   then 0
                   else ka *^ sum (map (^.intensity) $ config^.lights)
    diffColor = if kd == 0
                then 0
                else kd *^ sum (map calcDiff intensities)
    specColor = if ks == 0
                then 0
                else ks *^ sum (map calcSpec intensities)
{-# INLINE diffIllum #-}

reflIllum :: (Num a, Epsilon a, Floating a, Ord a, Num b, Eq b)
          => Config a b
          -> a          -- ^ Reflectance
          -> Solid a
          -> Ray a
          -> a
          -> Color a
reflIllum !config !ref !x !ray !t = if ref == 0
                                                then 0
                                                else ref *^ reflColor
  where
    reflRay = reflected x ray $ t - epsilon
    reflColor = trace config reflRay
{-# INLINE reflIllum #-}

refrIllum :: (Num a, Epsilon a, Floating a, Ord a, Num b, Eq b)
          => Config a b
          -> a            -- ^ Transparency
          -> a            -- ^ Refractive Index
          -> Solid a
          -> Ray a
          -> a
          -> Color a
refrIllum !config !trs !m !x !ray !t = if trs == 0
                                                   then 0
                                                   else trs *^ refrColor 
  where
    refrRay = refracted x ray (t + epsilon) m
    refrColor = fromMaybe 0 (trace config <$> refrRay)
 
{-# INLINE refrIllum #-}

-- =========================
