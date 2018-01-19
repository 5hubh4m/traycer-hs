module Traycer.Graphics.RayTracingSpec
  ( spec
  ) where

import Data.Foldable
import Data.Function
import Data.Maybe
import Control.Lens
import Linear.Epsilon
import Linear.Metric
import Linear.Vector
import Traycer.Math (epsilon, reflect)
import Traycer.Graphics.Color
import Traycer.Graphics.Body
import Traycer.Graphics.Light
import Traycer.Graphics.Texture
import Traycer.Geometry.Ray
import Traycer.Geometry.Solid
import Traycer.Config
import Traycer.Graphics.RayTracing
import Test.Hspec

spec :: Spec
spec = return ()
