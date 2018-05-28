{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns  #-}

module Traycer.Graphics.Rendering
  ( renderImage
  ) where

import Linear.V2
import Linear.V3
import Linear.Vector
import Linear.Epsilon
import Data.Word
import Data.Array.Repa hiding (map, (*^))
import Control.Lens
import Codec.Picture
import Traycer.Config
import Traycer.Graphics.Camera
import Traycer.Graphics.Color
import Traycer.Graphics.RayTracing
import Traycer.Geometry.Ray

type RGB8 = (Word8, Word8, Word8)

renderImage :: (Floating a, RealFrac a, Enum a, Epsilon a, Ord a, Eq a, Integral b)
            => Config a b -> DynamicImage
renderImage !c = toImage
                 $ runIdentity . computeP
                 $ transpose
                 $ fromFunction (Z :. height :. width) (pixelColor c aperture aa n)
  where
    width = fromIntegral $ c^.camera^.windowDim^._x :: Int
    height = fromIntegral $ c^.camera^.windowDim^._y :: Int
    (V2 w h) = pixelSize $ c^.camera
    aa = jitterSamples (c^.aaSamples) 0 w (-h) 0
    aperture = jitterSamples (c^.dofSamples) (-3 * w) (3 * w) (-3 * h) (3 * h)
    n = fromIntegral $ length aa * length aperture
{-# INLINE renderImage #-}

-- ======================
-- Temporary functions

pixelColor :: (Floating a, RealFrac a, Enum a, Epsilon a, Ord a, Eq a, Integral b)
           => Config a b
           -> [V3 a]
           -> [V3 a]
           -> b
           -> DIM2
           -> RGB8
pixelColor !c !aperture !aa !n (Z :. y :. x) = (pixel^.r, pixel^.g, pixel^.b)
  where
    pixelPos = pixel2Pos (c^.camera) (V2 (fromIntegral x) (fromIntegral y))
    primaryRay = (c^.camera^.eye) --> pixelPos $ 1
    aimedPoint = primaryRay *-> (c^.camera^.focalLength)
    eyePosSamples = map (+ (c^.camera^.eye)) aperture 
    aaSuperSamples = map (+ aimedPoint) aa
    eyeRays = [p --> t $ 1 | p <- eyePosSamples, t <- aaSuperSamples]
    pixel = toRGB $ sum (map (trace c) eyeRays) / fromIntegral n
{-# INLINE pixelColor #-}

toImage :: Array U DIM2 RGB8 -> DynamicImage
toImage !a = ImageRGB8 $ generateImage gen width height
  where
    Z :. width :. height = extent a
    gen x y = PixelRGB8 k l m
      where
        (k, l, m) = a ! (Z :. x :. y) 
{-# INLINE toImage #-}

jitterSamples :: (Enum a, Num a, Fractional a, Integral b)
              => b
              -> a
              -> a
              -> a
              -> a
              -> [V3 a] 
jitterSamples num w1 w2 h1 h2 = [V3 m n 0 | m <- xsamples, n <- ysamples]
  where
    numSamples = ceiling $ sqrt (fromIntegral num :: Double) :: Int
    xsamples = uniformSamples w1 w2 numSamples
    ysamples = uniformSamples h1 h2 numSamples
{-# INLINE jitterSamples #-}

uniformSamples :: (Enum a, Num a, Fractional a, Integral b) => a -> a -> b -> [a]
uniformSamples l u n = map (* width) $ take (fromIntegral n) [1 ..]
  where
   width = (u - l) / (1 + fromIntegral n)
{-# INLINE uniformSamples #-}

-- ======================
