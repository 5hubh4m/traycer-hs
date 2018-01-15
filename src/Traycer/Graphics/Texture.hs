{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE BangPatterns    #-}

module Traycer.Graphics.Texture
  ( Texture(..)
  , albedo
  , kAmbient
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

import GHC.Generics
import Traycer.Graphics.Color
import Control.Lens

data Texture a = Diffuse { _albedo :: !(Color a)
                         , _kAmbient :: !a
                         , _kDiffuse :: !a
                         , _kSpecular :: !a
                         , _specularExponent :: !a
                         }
               | Reflective { _albedo :: !(Color a)
                            , _kAmbient :: !a
                            , _kDiffuse :: !a
                            , _kSpecular :: !a
                            , _specularExponent :: !a
                            ,_reflectance :: !a
                            }
               | Transparent { _albedo :: !(Color a)
                             , _kAmbient :: !a
                             , _kDiffuse :: !a
                             , _kSpecular :: !a
                             , _specularExponent :: !a
                             , _reflectance :: !a
                             , _transparency :: !a
                             , _mu :: !a
                             }
               deriving (Show, Read, Eq, Generic)

makeLenses ''Texture

mkDiffuse :: (Num a, Ord a) => Color a -> a -> a -> a -> a -> Texture a
mkDiffuse !c !ka !kd !ks !n
  | ka < 0 || 1 < ka = error "Invalid value for ambient coefficient."
  | kd < 0 || 1 < kd = error "Invalid value for diffuse coefficient."
  | ks < 0 || 1 < ks = error "Invalid value for specular coefficient."
  | n < 0            = error "Invalid value for specular exponent."
  | otherwise        = Diffuse c ka kd ks n
{-# INLINE mkDiffuse #-}

mkReflective :: (Num a, Ord a) => Color a -> a -> a -> a -> a -> a -> Texture a
mkReflective !c !ka !kd !ks !n !ref
  | ka < 0 || 1 < ka   = error "Invalid value for ambient coefficient."
  | kd < 0 || 1 < kd   = error "Invalid value for diffuse coefficient."
  | ks < 0 || 1 < ks   = error "Invalid value for specular coefficient."
  | n < 0              = error "Invalid value for specular exponent."
  | ref < 0 || 1 < ref = error "Invalid value for reflectance."
  | otherwise = Reflective c ka kd ks n ref
{-# INLINE mkReflective #-}

mkTransparent :: (Num a, Ord a) => Color a -> a -> a -> a -> a -> a -> a -> a -> Texture a
mkTransparent !c !ka !kd !ks !n !ref !trs m
  | ka < 0 || 1 < ka   = error "Invalid value for ambient coefficient."
  | kd < 0 || 1 < kd   = error "Invalid value for diffuse coefficient."
  | ks < 0 || 1 < ks   = error "Invalid value for specular coefficient."
  | n < 0              = error "Invalid value for specular exponent."
  | ref < 0 || 1 < ref = error "Invalid value for reflectance."
  | trs < 0 || 1 < trs = error "Invalid value for transparency."
  | m < 1              = error "Invalid value for refractive index."
  | otherwise          = Transparent c ka kd ks n ref trs m 
{-# INLINE mkTransparent #-}
