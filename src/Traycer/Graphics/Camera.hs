{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE BangPatterns    #-}

module Traycer.Graphics.Camera
  ( Camera
  , mkCamera
  , eye
  , windowSize
  , windowDim
  , focalLength
  , windowCenter
  , xVector
  , yVector
  , pixelSize
  , pixel2Pos
  ) where

import GHC.Generics
import Control.Lens
import Linear.V3
import Linear.V2
import Linear.Vector
import Linear.Metric
import Linear.Epsilon

data Camera a b = Camera { _eye :: !(V3 a)            -- ^ Location of eye in the scene
                         , _windowSize :: !(V2 a)     -- ^ Size of window in the scene world
                         , _windowDim :: !(V2 b)      -- ^ Dimension of window in pixels
                         , _focalLength :: !a
                         , _windowCenter :: !(V3 a)   -- ^ Center of the window
                         , _xVector :: !(V3 a)        -- ^ X vector along the plane of window
                         , _yVector :: !(V3 a)        -- ^ Y vector along the plane of the window
                         }
                deriving (Show, Read, Eq, Generic)

makeLenses ''Camera

mkCamera :: (Epsilon a, Floating a, Ord a, Num b, Ord b)
         => V3 a -- Eye location
         -> V2 a -- Size of window
         -> V3 a -- Window's center
         -> V3 a -- Normal to window plane
         -> V3 a -- Rotation of window in it's plane
         -> V2 b -- Dimension of window
         -> a    -- Focal length
         -> Camera a b
mkCamera !p (V2 !s1 !s2) !c !wn !wr (V2 !d1 !d2) !f
  | s1 <= 0 || s2 <= 0 || d1 <= 0 || d2 <= 0 = error "Dimension values less than 0."
  | f <= 0                                   = error "Invalid focal length."   
  | nearZero wn || nearZero wr               = error "Normal vector can't be zero."
  | otherwise                                = Camera p (V2 s1 s2) (V2 d1 d2) f c x y
  where
    y = normalize $ cross x wn 
    x = normalize $ cross wn wr
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
  | otherwise                  = (camera^.windowCenter) +
                                 xlen *^ (camera^.xVector) +
                                 ylen *^ (camera^.yVector)
  where
    xscale = camera^.windowSize^._x / fromIntegral (camera^.windowDim^._x)
    yscale = camera^.windowSize^._y / fromIntegral (camera^.windowDim^._y)
    xlen = xscale * fromIntegral x - (camera^.windowSize^._x / 2)
    ylen = (camera^.windowSize^._y / 2) - yscale * fromIntegral y
{-# INLINE pixel2Pos #-}
