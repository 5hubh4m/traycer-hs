{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings    #-}

module Traycer.Parser
  where

import Data.Yaml
import Data.Aeson
import Data.Vector((!))
import Linear.V3
import Linear.V2
import Linear.Epsilon
import Traycer.Config
import Traycer.Graphics.Body
import Traycer.Graphics.Color
import Traycer.Graphics.Camera
import Traycer.Graphics.Light
import Traycer.Graphics.Texture
import Traycer.Geometry.Solid

parseOptions :: Options
parseOptions = defaultOptions { fieldLabelModifier = drop 1
                              , unwrapUnaryRecords = True
                              }

instance (FromJSON a) => FromJSON (V3 a) where
  parseJSON = genericParseJSON parseOptions

instance (FromJSON a) => FromJSON (V2 a) where
  parseJSON = genericParseJSON parseOptions

instance (Num a, Ord a, FromJSON a) => FromJSON (Color a) where
  parseJSON = withArray "Array of Colors in [0, 1]" $ \arr ->
    mkColor <$> parseJSON (arr ! 0)
            <*> parseJSON (arr ! 1)
            <*> parseJSON (arr ! 2)
  
instance (Ord a, Floating a, Epsilon a, FromJSON a) => FromJSON (Solid a) where
  parseJSON = withObject "Solid" $ \o -> do
    typ <- o .: "type"
    case typ of
      "Sphere" -> mkSphere <$> o .: "center"
                           <*> o .: "radius"
      "Plane" -> mkPlane <$> o .: "center"
                         <*> o .: "normal"
      "Disk" -> mkDisk <$> o .: "center"
                       <*> o .: "normal"
                       <*> o .: "radius"
      _ -> fail $ "Unknown solid: " ++ typ

instance (Num a, Ord a, FromJSON a) => FromJSON (Texture a) where
  parseJSON = withObject "Texture" $ \o -> do
    typ <- o .: "type"
    case typ of
      "Diffuse" -> mkDiffuse <$> o .: "albedo"
                             <*> o .: "kAmbient"
                             <*> o .: "kDiffuse"
                             <*> o .: "kSpecular"
                             <*> o .: "specularExponent"
      "Reflective" -> mkReflective <$> o .: "albedo"
                                   <*> o .: "kAmbient"
                                   <*> o .: "kDiffuse"
                                   <*> o .: "kSpecular"
                                   <*> o .: "specularExponent"
                                   <*> o .: "reflectance"
      "Transparent" -> mkTransparent <$> o .: "albedo"
                                     <*> o .: "kAmbient"
                                     <*> o .: "kDiffuse"
                                     <*> o .: "kSpecular"
                                     <*> o .: "specularExponent"
                                     <*> o .: "reflectance"
                                     <*> o .: "transparency"
                                     <*> o .: "mu"
      _ -> fail $ "Unknown texture: " ++ typ

instance (Num a, Ord a, FromJSON a) => FromJSON (Light a) where
  parseJSON = genericParseJSON parseOptions

instance (Ord a, Floating a, Epsilon a, FromJSON a) => FromJSON (Body a) where
  parseJSON = genericParseJSON parseOptions

instance (Num a, Ord a, Num b, Ord b, FromJSON a, FromJSON b) => FromJSON (Camera a b) where
  parseJSON = withObject "Camera" $ \o ->
    mkCamera <$> o .: "eye"
             <*> o .: "windowSize"
             <*> o .: "windowDim"
             <*> o .: "focalLength"

instance (Ord a, Floating a, Epsilon a, FromJSON a, Num b, Ord b, FromJSON b) => FromJSON (Config a b) where
  parseJSON = withObject "Config" $ \o ->
    mkConfig <$> o .: "bodies"
             <*> o .: "lights"
             <*> o .: "ambient"
             <*> o .: "camera"
             <*> o .: "depth"
             <*> o .: "aaSamples"
             <*> o .: "dofSamples"
