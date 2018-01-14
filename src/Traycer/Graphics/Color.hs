{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Traycer.Graphics.Color
  ( Color
  , color
  , mkColor
  , fromV3
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
import GHC.Generics
import Control.Lens
import Data.Word

newtype Color a = Color { _color :: V3 a }
                deriving ( Show,
                           Read,
                           Eq,
                           Ord,
                           Functor,
                           Num,
                           Fractional,
                           Floating,
                           Generic
                         )

makeLenses ''Color

r :: Lens' (Color a) a
r = color . _x
{-# INLINE r #-}

g :: Lens' (Color a) a 
g = color ._y
{-# INLINE g #-}

b :: Lens' (Color a) a
b = color ._z
{-# INLINE b #-}

mkColor :: (Num a, Ord a) => a -> a -> a -> Color a
mkColor m n o
  | m < 0 ||
    m > 1 ||
    n < 0 ||
    n > 1 ||
    o < 0 ||
    o > 1     = error "Invalid values of color coordinates."
  | otherwise = Color $ V3 m n o
{-# INLINE mkColor #-}

fromV3 :: (Num a, Ord a) => V3 a -> Color a
fromV3 v = mkColor (v^._x) (v^._y) (v^._z)

fromRGB :: (Num a, Ord a, Fractional a) => Word8 -> Word8 -> Word8 -> Color a
fromRGB x y z = mkColor (f x) (f y) (f z)
  where
    f a = fromIntegral a / 255
{-# INLINE fromRGB #-}

toRGB :: (RealFrac a) => Color a -> Color Word8
toRGB c = (round . (*) 255) <$> c
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
clip c = Color $ (\x -> if x > 1 then 1 else if x < 0 then 0 else x) <$> (c^.color)
{-# INLINE clip #-}
