{-# LANGUAGE AllowAmbiguousTypes #-}

module Traycer.Graphics.RayTracing
  ( trace
  , collide
  ) where

import Data.Foldable
import Data.Function
import Data.Maybe
import Control.Lens
import Linear.Epsilon
import Linear.Metric
import Linear.Vector
import Traycer.Math.Misc
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
collide bs ray
  | null ts   = Nothing
  | otherwise = Just $ minimumBy (compare `on` fst) ts
  where
    ts =  map (_1%~fromMaybe 0)
          $ filter (isJust . fst)
          $ map (\x -> ((x^.solid) `hit` ray, x)) bs
{-# INLINE collide #-}

-- | The "engine" of the ray-tracer
--   This recursive function will trace a give ray
trace :: (Show a, Epsilon a, Floating a, Eq a, Ord a, Num b, Eq b)
      => Config a b          -- ^ Configuration of the scene
      -> Ray a               -- ^ Ray to trace
      -> Color a             -- ^ Color of ray traced
trace config ray
  | config^.depth == 0 = config^.ambient
  | otherwise          = case collide (config^.bodies) ray of
      Nothing     -> config ^.ambient
      Just (t, x) -> let
        -- | Point of intersection and normal
        p = ray *-> (t - epsilon)
        n = normalAt (x^.solid) ray t

        -- | Rays from the point of intersection to light sources
        shadowRays = map (\l -> (l, p --> (l^.position) $ 1)) $ config^.lights
        lightRays = filter (isNothing . collide (config^.bodies) . snd) shadowRays

        -- | Color of a diffuse surface using phong model
        diffColor c = sum $
          map
          (\(l, lray) -> c * (l^.intensity) ^* dot n (lray^.direction))
          lightRays
        specColor e = sum $
          map
          (\(l, lray) -> (l^.intensity) ^* abs (dot (reflect (-lray^.direction) n) (ray^.direction)) ** e)
          lightRays

        -- | New config for recursion
        config' = config&depth %~ subtract 1

        -- | Color of the reflected ray
        reflColor = trace config' $ reflected (x^.solid) ray (t - epsilon)

        -- | Color of the transmitted ray
        refrColor m = fromMaybe 0 $ trace config' <$> refracted (x^.solid) ray (t + epsilon) m
        eta m = if not $ isIn (x^.solid) (ray^.origin) then m else 1
        in case x^.texture of
             Diffuse c kd ks e     -> clip $ kd *^ diffColor c + ks *^ specColor e 
             Reflective ref        -> if ref == 0 then 0 else ref *^ reflColor
             Transparent ref trs m -> clip $ (if ref == 0 then 0 else ref *^ reflColor)
                                           + (if trs == 0 then 0 else trs *^ refrColor (eta m))
{-# INLINE trace #-}
