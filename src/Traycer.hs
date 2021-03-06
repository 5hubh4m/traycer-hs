module Traycer
  ( Config(), mkConfigFromTextures, mkConfig
  , Body()
  , mkBody
  , Light(..)
  , Camera(), mkCamera
  , Color(), mkColor, fromRGB, _white, _black, _red, _green, _blue
  , Texture(), mkPlain
  , Solid(), mkSphere, mkPlane, mkDisk, mkPolyFromVertices, mkCuboid, mkRectangle
  , Transform(), mkRotation, mkTranslation
  , renderImage
  , V3(..)
  , V2(..)
  ) where

import Linear.V2
import Linear.V3
import Traycer.Config
import Traycer.Parser()
import Traycer.Graphics.Body
import Traycer.Graphics.Light
import Traycer.Graphics.Camera
import Traycer.Graphics.Color
import Traycer.Graphics.Texture
import Traycer.Graphics.Rendering
import Traycer.Geometry.Solid
import Traycer.Geometry.Transform
