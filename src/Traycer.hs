{-# LANGUAGE TypeOperators, FlexibleContexts #-}

module Traycer
  ( Config, mkConfig
  , Body(..)
  , Light(..)
  , mkCamera
  , mkColor, fromRGB, _white, _black, _red, _green, _blue
  , mkDiffuse, mkReflective, mkTransparent
  , trace
  , mkSphere, mkPlane, mkDisk
  , config2Image
  , V3(..)
  , V2(..)
  ) where

import Linear.V2
import Linear.V3
import Data.Word
import Data.Array.Repa hiding (map)
import Control.Lens
import Codec.Picture
import System.Random
import Traycer.Config
import Traycer.Graphics.Body
import Traycer.Graphics.Light
import Traycer.Graphics.Camera
import Traycer.Graphics.Color
import Traycer.Graphics.Texture
import Traycer.Graphics.RayTracing
import Traycer.Geometry.Ray
import Traycer.Geometry.Solid

type RGB8 = (Word8, Word8, Word8)

config2Image :: Config Double Int
             -> FilePath
             -> IO ()
config2Image c name = do
  let width = c^.camera^.windowDim^._x
  let height = c^.camera^.windowDim^._y
  gen <- getStdGen
  im <- computeUnboxedP $ transpose $ fromFunction (Z :. height :. width) $ pixelColor c gen
  savePngImage name $ toImage im

pixelColor :: Config Double Int
           -> StdGen
           -> DIM2
           -> RGB8
pixelColor c gen (Z :. y :. x) = let
  (V2 w h) = pixelSize $ c^.camera
  eyePos = c^.camera^.eye
  pixelPos = pixel2Pos (c^.camera) (V2 x y)
  primaryRay = eyePos --> pixelPos $ 1
  aimedPoint = primaryRay *-> (c^.camera^.focalLength)
  eyePosSamples = map ((+ eyePos) . (\(m, n) -> V3 (w * m) (h * n) 0)) $
                  list2Zip $ take (2 * c^.dofSamples) $ randomRs (-1, 1) gen
  antiAliasingSamples = map ((+ aimedPoint) . (\(m, n) -> V3 (w * m) (h * n) 0)) $
                        list2Zip $ take (2 * c^.aaSamples) $ randomRs (0, w) gen
  eyeRays = [p --> t $ 1 | p <- eyePosSamples, t <- antiAliasingSamples]
  pixel = toRGB $ sum (map (trace c) eyeRays) /
                  fromIntegral (length antiAliasingSamples * length eyePosSamples)
  in (pixel^.r, pixel^.g, pixel^.b)
  where
    list2Zip [] = []
    list2Zip (x1:x2:xs) = (x1, x2) : list2Zip xs
    list2Zip _ = error "Odd length list"
    {-# INLINE list2Zip #-}
{-# INLINE pixelColor #-}

toImage :: Array U DIM2 RGB8 -> DynamicImage
toImage a = ImageRGB8 $ generateImage gen width height
  where
    Z :. width :. height = extent a
    gen x y =
      let (k, l, m) = a ! (Z :. x :. y)
      in PixelRGB8 k l m
    {-# INLINE gen #-}
{-# INLINE toImage #-}
