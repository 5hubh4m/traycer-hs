{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE BangPatterns               #-}

module Traycer.Graphics.Color
  ( Color()
  , mkColor
  , fromRGB
  , toRGB
  , r
  , g
  , b
  , clip
  , _white
  , _black
  , _red
  , _green
  , _blue
  ) where

import Linear.V3
import Control.Lens
import Data.Word

type Color a = V3 a

r :: Lens' (Color a) a
r = _x
{-# INLINE r #-}

g :: Lens' (Color a) a 
g = _y
{-# INLINE g #-}

b :: Lens' (Color a) a
b = _z
{-# INLINE b #-}

mkColor :: (Num a, Ord a) => a -> a -> a -> Color a
mkColor !m !n !o
  | m < 0 ||
    m > 1 ||
    n < 0 ||
    n > 1 ||
    o < 0 ||
    o > 1     = error "Invalid values of color coordinates."
  | otherwise = V3 m n o
{-# INLINE mkColor #-}

fromRGB :: (Num a, Ord a, Fractional a) => Word8 -> Word8 -> Word8 -> Color a
fromRGB !x !y !z = mkColor (f x) (f y) (f z)
  where
    f !a = fromIntegral a / 255
{-# INLINE fromRGB #-}

toRGB :: (RealFrac a) => Color a -> Color Word8
toRGB !c = (round . (*) 255) <$> c
{-# INLINE toRGB #-}

_white :: (Num a, Ord a) => Color a
_white = mkColor 1 1 1
{-# INLINE _white #-}

_black :: (Num a, Ord a) => Color a
_black = mkColor 0 0 0
{-# INLINE _black #-}

_red :: (Num a, Ord a) => Color a
_red = mkColor 1 0 0
{-# INLINE _red #-}

_green :: (Num a, Ord a) => Color a
_green = mkColor 0 1 0
{-# INLINE _green #-}

_blue :: (Num a, Ord a) => Color a
_blue = mkColor 0 0 1
{-# INLINE _blue #-}

clip :: (Num a, Ord a) => Color a -> Color a
clip !c = f <$> c
  where
    f x
      | x < 0     = 0
      | x > 1     = 1
      | otherwise = x
{-# INLINE clip #-}
