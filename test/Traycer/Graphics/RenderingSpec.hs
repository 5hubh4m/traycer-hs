module Traycer.Graphics.RenderingSpec
  ( spec
  ) where

import Linear.V2
import Linear.V3
import Linear.Epsilon
import Data.Word
import Data.Array.Repa hiding (map)
import Control.Lens
import Codec.Picture
import Traycer.Config
import Traycer.Graphics.Camera
import Traycer.Graphics.Color
import Traycer.Graphics.RayTracing
import Traycer.Geometry.Ray
import Traycer.Graphics.Rendering
import Test.Hspec

spec :: Spec
spec = return ()
