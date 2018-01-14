{-# OPTIONS_GHC -fno-warn-orphans #-}

module Traycer.Parser
  where

import Data.Yaml
import Linear.V3
import Linear.V2
import Traycer.Config
import Traycer.Graphics.Body
import Traycer.Graphics.Color
import Traycer.Graphics.Camera
import Traycer.Graphics.Light
import Traycer.Graphics.Texture
import Traycer.Geometry.Solid

instance (FromJSON a) => FromJSON (V3 a) where
instance (FromJSON a) => FromJSON (V2 a) where
instance (FromJSON a) => FromJSON (Color a) where
instance (FromJSON a) => FromJSON (Solid a) where
instance (FromJSON a) => FromJSON (Texture a) where
instance (FromJSON a) => FromJSON (Light a) where
instance (FromJSON a) => FromJSON (Body a)
instance (FromJSON a, FromJSON b) => FromJSON (Camera a b) where
instance (FromJSON a, FromJSON b) => FromJSON (Config a b) where
