{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE BangPatterns    #-}

module Traycer.Graphics.Texture
  ( Texture()
  , albedo
  , kAmbient
  , kDiffuse
  , kSpecular
  , specularExponent
  , reflectance
  , transparency
  , mu
  , mkPlain
  ) where

import GHC.Generics
import Traycer.Graphics.Color
import Control.Lens

data Texture a = Plain { _albedo :: !(Color a)
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

mkPlain :: (Num a, Ord a) => Color a -> a -> a -> a -> a -> a -> a -> a -> Texture a
mkPlain !c !ka !kd !ks !n !ref !trs m
  | ka < 0 || 1 < ka   = error "Invalid value for ambient coefficient."
  | kd < 0 || 1 < kd   = error "Invalid value for diffuse coefficient."
  | ks < 0 || 1 < ks   = error "Invalid value for specular coefficient."
  | n < 0              = error "Invalid value for specular exponent."
  | ref < 0 || 1 < ref = error "Invalid value for reflectance."
  | trs < 0 || 1 < trs = error "Invalid value for transparency."
  | m < 1              = error "Invalid value for refractive index."
  | otherwise          = Plain c ka kd ks n ref trs m 
{-# INLINE mkPlain #-}
