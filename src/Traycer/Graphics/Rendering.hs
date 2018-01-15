{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns  #-}

module Traycer.Graphics.Rendering
  ( renderImage
  ) where

import Linear.V2
import Linear.V3
import Linear.Epsilon
import Data.Word
import Data.Array.Repa hiding (map)
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
                 $ fromFunction (Z :. height :. width) (pixelColor c)
  where
    width = fromIntegral $ c^.camera^.windowDim^._x :: Int
    height = fromIntegral $ c^.camera^.windowDim^._y :: Int
{-# INLINE renderImage #-}

uniformSamples :: (Enum a, Num a, Fractional a, Integral b) => (a, a) -> b -> [a]
uniformSamples (l, u) n = map (* width) $ take (fromIntegral n) [1 ..]
  where
   width = (u - l) / (1 + fromIntegral n)
{-# INLINE uniformSamples #-}

apertureSamples :: (Enum a, Num a, Fractional a, Integral b)
                => Config a b -> [V3 a]
apertureSamples config = [V3 m n 0 | m <- xsamples, n <- ysamples]
  where
    (V2 w h) = pixelSize $ config^.camera
    numSamples = ceiling $ sqrt (fromIntegral $ config^.dofSamples :: Double) :: Int
    xsamples = uniformSamples (-3 * w, 3 * w) numSamples
    ysamples = uniformSamples (-3 * h, 3 * h) numSamples
{-# INLINE apertureSamples #-}

antiAliasingSamples :: (Enum a, Num a, Fractional a, Integral b)
                    => Config a b -> [V3 a] 
antiAliasingSamples config = [V3 m n 0 | m <- xsamples, n <- ysamples]
  where
    (V2 w h) = pixelSize $ config^.camera
    numSamples = ceiling $ sqrt (fromIntegral $ config^.aaSamples :: Double) :: Int
    xsamples = uniformSamples (0, w) numSamples
    ysamples = uniformSamples (-h, 0) numSamples
{-# INLINE antiAliasingSamples #-}

pixelColor :: (Floating a, RealFrac a, Enum a, Epsilon a, Ord a, Eq a, Integral b)
           => Config a b
           -> DIM2
           -> RGB8
pixelColor !c (Z :. y :. x) = (pixel^.r, pixel^.g, pixel^.b)
  where
    eyePos = c^.camera^.eye
    pixelPos = pixel2Pos (c^.camera) (V2 (fromIntegral x) (fromIntegral y))
    primaryRay = eyePos --> pixelPos $ 1
    aimedPoint = primaryRay *-> (c^.camera^.focalLength)
    eyePosSamples = map (+ eyePos) $ apertureSamples c
    aaSuperSamples = map (+ aimedPoint) $ antiAliasingSamples c
    eyeRays = [p --> t $ 1 | p <- eyePosSamples, t <- aaSuperSamples]
    pixel = toRGB $ sum (map (trace c) eyeRays) /
                    fromIntegral (length aaSuperSamples * length eyePosSamples)
{-# INLINE pixelColor #-}

toImage :: Array U DIM2 RGB8 -> DynamicImage
toImage !a = ImageRGB8 $ generateImage gen width height
  where
    Z :. width :. height = extent a
    gen x y =
      let (k, l, m) = a ! (Z :. x :. y)
      in PixelRGB8 k l m
    {-# INLINE gen #-}
{-# INLINE toImage #-}
