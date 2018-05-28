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
import Traycer.Geometry.Transform

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
      "Sphere" -> mkSphere <$> o .: "radius"
      "Plane" -> mkPlane <$> o .: "normal"
      "Disk" -> mkDisk <$> o .: "normal"
                       <*> o .: "radius"
      "Poly" -> mkPoly <$> o .: "vertices"
      "Cuboid" -> mkCuboid <$> o .: "xLength"
                           <*> o .: "yLength"
                           <*> o .: "zLength"
      _ -> fail $ "Unknown solid: " ++ typ

instance (Ord a, Floating a, Epsilon a, FromJSON a) => FromJSON (Transform a) where
  parseJSON = withObject "Transform" $ \o -> do
    typ <- o .: "type"
    case typ of
      "Rotation" -> mkRotation <$> o .:? "xAngle" .!= 0
                               <*> o .:? "yAngle" .!= 0
                               <*> o .:? "zAngle" .!= 0
                               <*> o .:? "center" .!= 0
      "Translation" -> mkTranslation <$> o .: "vector"
      _ -> fail $ "Unknown transform: " ++ typ

instance (Fractional a, Ord a, FromJSON a) => FromJSON (Texture a) where
  parseJSON = withObject "Texture" $ \o ->
    mkPlain <$> o .: "albedo"
            <*> o .:? "kAmbient" .!= 0.05
            <*> o .:? "kDiffuse" .!= 0.9
            <*> o .:? "kSpecular" .!= 0.1
            <*> o .:? "exponent" .!= 10
            <*> o .:? "reflectance" .!= 0
            <*> o .:? "transparency" .!= 0
            <*> o .:? "mu" .!= 1

instance (Num a, Ord a, FromJSON a) => FromJSON (Light a) where
  parseJSON = withObject "Light" $ \o ->
    Light <$> o .: "intensity"
          <*> o .: "position"

instance (Ord a, Floating a, Epsilon a, FromJSON a) => FromJSON (Body a) where
  parseJSON = withObject "Body" $ \o ->
    mkBody <$> o .: "solid"
           <*> o .: "texture"
           <*> o .:? "transforms" .!= []
  
instance (Epsilon a, Floating a, Ord a, Num b, Ord b, FromJSON a, FromJSON b) => FromJSON (Camera a b) where
  parseJSON = withObject "Camera" $ \o ->
    mkCamera <$> o .:? "eye" .!= V3 0 0 (-10)
             <*> o .: "windowSize"
             <*> o .: "windowDim"
             <*> o .:? "focalLength" .!= 10

instance (Ord a, Floating a, Epsilon a, FromJSON a, Num b, Ord b, FromJSON b) => FromJSON (SolidRef a b) where
  parseJSON = withObject "Body" $ \o ->
    SolidRef <$> o .: "solid"
             <*> o .: "texture"
             <*> o .:? "transforms" .!= []

instance (Ord a, Floating a, Epsilon a, FromJSON a, Num b, Ord b, Integral b, FromJSON b) => FromJSON (Config a b) where
  parseJSON = withObject "Config" $ \o ->
    mkConfigFromTextures <$> o .: "materials"
                         <*> o .: "bodies"
                         <*> o .: "lights"
                         <*> o .:? "ambient" .!= _white
                         <*> o .: "camera"
                         <*> o .:? "depth" .!= 5
                         <*> o .:? "aaSamples" .!= 4
                         <*> o .:? "dofSamples" .!= 4
                         <*> o .:? "transforms" .!= []
