{-# LANGUAGE TypeOperators #-}

module Traycer
  ( Config(..)
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
import qualified Data.Array.Repa as R
import Data.Array.Repa.IO.BMP
import Control.Lens
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

config2Image :: Config Double Int
             -> FilePath
             -> IO ()
config2Image c name = do
  gen <- getStdGen
  im <- R.computeP $ R.fromFunction
        ( R.Z
          R.:. (c^.camera^.windowDim^._y)
          R.:. (c^.camera^.windowDim^._x)
        ) 
        (\(R.Z R.:. y R.:. x) -> let (V2 w h) = pixelSize $ c^.camera
                                     eyePos = c^.camera^.eye
                                     pixelPos = pixel2Pos (c^.camera) (V2 x y)
                                     primaryRay = eyePos --> pixelPos $ 1
                                     aimedPoint = primaryRay *-> (c^.camera^.focalLength)
                                     eyePosSamples = map ((+ eyePos) . (\(m, n) -> V3 (w * m) (h * n) 0)) $
                                                     list2Zip $ take 50 $ randomRs (-1 :: Double, 1) gen
                                     antiAliasingSamples = map ((+ aimedPoint) . (\(m, n) -> V3 (w * m) (h * n) 0)) $
                                                           list2Zip $ take 20 $ randomRs (0, w) gen
                                     eyeRays = [p --> t $ 1 | p <- eyePosSamples, t <- antiAliasingSamples]
                                     pixelColor = toRGB $ sum (map (trace c) eyeRays) /
                                                  fromIntegral (length antiAliasingSamples * length eyePosSamples)
                                 in (pixelColor^.r, pixelColor^.g, pixelColor^.b))
  writeImageToBMP name im
    where
      list2Zip [] = []
      list2Zip (x1:x2:xs) = (x1, x2) : list2Zip xs
      list2Zip _ = error "Odd length list"
