{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE BangPatterns    #-}

module Traycer.Graphics.Camera
  ( Camera()
  , mkCamera
  , eye
  , windowSize
  , windowDim
  , focalLength
  , pixelSize
  , pixel2Pos
  ) where

import GHC.Generics
import Control.Lens
import Linear.V3
import Linear.V2

data Camera a b = Camera { _eye :: !(V3 a)            -- ^ Location of eye in the scene
                         , _windowSize :: !(V2 a)     -- ^ Size of window in the scene world
                         , _windowDim :: !(V2 b)      -- ^ Dimension of window in pixels
                         , _focalLength :: !a
                         }
                deriving (Show, Read, Eq, Generic)

makeLenses ''Camera

mkCamera :: (Num a, Ord a, Num b, Ord b)
         => V3 a -- Eye location
         -> V2 a -- Size of window
         -> V2 b -- Dimension of window
         -> a    -- Focal length
         -> Camera a b
mkCamera !p (V2 !s1 !s2) (V2 !d1 !d2) !f
  | s1 <= 0 || s2 <= 0 || d1 <= 0 || d2 <= 0 = error "Dimension values less than 0."
  | f <= 0                                   = error "Invalid focal length."   
  | otherwise                                = Camera p (V2 s1 s2) (V2 d1 d2) f
{-# INLINE mkCamera #-}

pixelSize :: (Num a, Fractional a, Integral b) => Camera a b -> V2 a
pixelSize !camera = V2 (camera^.windowSize^._x / fromIntegral (camera^.windowDim^._x))
                       (camera^.windowSize^._y / fromIntegral (camera^.windowDim^._y))
{-# INLINE pixelSize #-}

-- | Assuming the view window is located such that
--   the lower left corner is at the origin
--   in the XY plane, given the pixel coordinate,
--   find the position in world coordinates
pixel2Pos :: (Num a, Fractional a, Integral b, Num b, Ord b)
          => Camera a b
          -> V2 b
          -> V3 a
pixel2Pos !camera (V2 !x !y)
  | x < 0                     ||
    y < 0                     ||
    x > camera^.windowDim^._x ||
    y > camera^.windowDim^._y  = error "Pixel coordinates out-of-bounds."
  | otherwise                  = V3 xlen ylen 0
  where
    xscale = camera^.windowSize^._x / fromIntegral (camera^.windowDim^._x)
    yscale = camera^.windowSize^._y / fromIntegral (camera^.windowDim^._y)
    xlen = (camera^.windowSize^._x / 2) - xscale * fromIntegral x
    ylen = (camera^.windowSize^._y / 2) - yscale * fromIntegral y
{-# INLINE pixel2Pos #-}
