module Traycer.ParserSpec
  ( spec
  ) where

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
import Traycer.Parser
import Test.Hspec

spec :: Spec
spec = return ()
