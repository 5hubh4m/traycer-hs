{-# LANGUAGE TemplateHaskell #-}

module Traycer.Graphics.Texture
  ( Texture(..)
  , color
  , kDiffuse
  , kSpecular
  , specularExponent
  , reflectance
  , transparency
  , mu
  , mkDiffuse
  , mkReflective
  , mkTransparent
  ) where

import Traycer.Graphics.Color
import Control.Lens

data Texture a = Diffuse { _color :: !(Color a)
                         , _kDiffuse :: !a
                         , _kSpecular :: !a
                         , _specularExponent :: !a
                         }
               | Reflective { _reflectance :: !a }
               | Transparent { _reflectance :: !a
                             , _transparency :: !a
                             , _mu :: !a
                             }
               deriving (Show, Eq)

makeLenses ''Texture

mkDiffuse :: (Num a, Ord a) => Color a -> a -> a -> a -> Texture a
mkDiffuse c kd ks n
  | kd < 0 || 1 < kd = error "Invalid value for diffuse coefficient."
  | ks < 0 || 1 < ks = error "Invalid value for specular coefficient."
  | n < 0            = error "Invalid value for specular exponent."
  | otherwise        = Diffuse c kd ks n
{-# INLINE mkDiffuse #-}

mkReflective :: (Num a, Ord a) => a -> Texture a
mkReflective ref
  | ref < 0 || 1 < ref = error "Invalid value for reflectance."
  | otherwise = Reflective ref
{-# INLINE mkReflective #-}

mkTransparent :: (Num a, Ord a) => a -> a -> a -> Texture a
mkTransparent ref trs m
  | ref < 0 || 1 < ref = error "Invalid value for reflectance."
  | trs < 0 || 1 < trs = error "Invalid value for transparency."
  | m < 1              = error "Invalid value for refractive index."
  | otherwise          = Transparent ref trs m 
{-# INLINE mkTransparent #-}
