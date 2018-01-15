{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}
module Traycer.Graphics.RayTracing
  ( trace ) where

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
  | otherwise = Just $ minimumBy (compare `on` fst) ts
  where
    ts = map (_1%~fromMaybe 0)
         $ filter (isJust . fst)
         $ map (\x -> ((x^.solid) `hit` ray, x)) bs
{-# INLINE collide #-}

-- | The "engine" of the ray-tracer
--   This recursive function will trace a give ray
trace :: (Epsilon a, Floating a, Eq a, Ord a, Num b, Eq b)
      => Config a b          -- ^ Configuration of the scene
      -> Ray a               -- ^ Ray to trace
      -> Color a             -- ^ Color of ray traced
trace !config !ray
  | config^.depth == 0 = config^.ambient
  | otherwise          = case collide (config^.bodies) ray of
      Nothing       -> config ^.ambient
      Just (t, x) -> case x^.texture of
        Diffuse c ka kd ks e -> phongColor
          where
            phongColor = diffuseIllumination config c ka kd ks e (x^.solid) ray t
        Reflective c ka kd ks e ref -> clip $ phongColor + reflColor
          where
            phongColor = diffuseIllumination config c ka kd ks e (x^.solid) ray t
            (_, reflColor) = reflectedIllumination config ref (x^.solid) ray t
        Transparent c ka kd ks e ref trs m -> clip $ phongColor + reflColor + refrColor
          where
            phongColor = diffuseIllumination config c ka kd ks e (x^.solid) ray t
            (config', reflColor) = reflectedIllumination config ref (x^.solid) ray t
            refrColor =
              if trs == 0
              then 0
              else trs *^ fromMaybe 0
                          (
                            trace config'
                            <$> refracted (x^.solid) ray (t + epsilon) m
                          )
{-# INLINE trace #-}

-- ==========================
-- Temporary functions

diffuseIllumination
  :: (Num a, Epsilon a, Floating a, Ord a, Num b, Eq b)
  => Config a b -> Color a -> a -> a -> a -> a -> Solid a -> Ray a -> a
  -> Color a
diffuseIllumination !config !c !ka !kd !ks !e !x !ray !t =
  clip $ ambientColor + diffColor + specColor
  where
    p = ray *-> (t - epsilon)
    n = normalAt x ray t
  
    -- | Rays from the point of intersection to light sources
    shadowRays = map (\l -> (l, p --> (l^.position) $ 1)) $ config^.lights
    lightRays = filter (isNothing . collide (config^.bodies) . snd) shadowRays
    ls = map (\(l, lray) -> (l^.intensity, lray^.direction)) lightRays
    bs = map (\(l, lray) -> (l^.intensity, lray^.direction)) shadowRays
    
    -- | Color of a diffuse surface using phong model
    ambientColor = if ka == 0
                   then 0
                   else ka *^ ((config^.ambient) + sum (map fst bs))
    diffColor = if kd == 0
                then 0
                else kd *^ sum (map (\(i, d) -> c * i ^* dot n d) ls)
    specColor =
      if ks == 0
      then 0
      else ks *^ sum (map (\(i, d) -> i ^* abs (dot (reflect d n) (ray^.direction)) ** e) ls)
{-# INLINE diffuseIllumination #-}

reflectedIllumination
  :: (Num a, Epsilon a, Floating a, Ord a, Num b, Eq b)
  => Config a b -> a -> Solid a -> Ray a -> a
  -> (Config a b, Color a)
reflectedIllumination !config !ref !x !ray !t = (config', reflColor)
  where
    config' = config&depth %~ subtract 1
    reflColor = if ref == 0
                then 0
                else ref *^ trace config' (reflected x ray $ t - epsilon)
{-# INLINE reflectedIllumination #-}

-- =========================
