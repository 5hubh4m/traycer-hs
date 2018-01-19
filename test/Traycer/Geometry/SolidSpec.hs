module Traycer.Geometry.SolidSpec
  ( spec
  ) where

import Control.Lens
import GHC.Generics
import Linear.Epsilon
import Linear.Metric
import Linear.V3
import Traycer.Geometry.Ray
import Traycer.Math
import Traycer.Geometry.Solid
import Test.Hspec

spec :: Spec
spec = return ()

  -- describe "isIn" $ do
  --   it "should say whether point is inside solid" $ do
  --     isIn () () `shouldBe` True
  --     isIn () () `shouldBe` True
  --     isIn () () `shouldBe` True
  --     isIn () () `shouldBe` False
  --     isIn () () `shouldBe` False
  --     isIn () () `shouldBe` False

  -- describe "hit" $ do
  --   it "should give the point that hits the solid" $ do
  --     hit () () `shouldSatisfy` isJust
  --     hit () () `shouldSatisfy` isJust
  --     hit () () `shouldSatisfy` isJust
  --     hit () () `shouldSatisfy` isNothing
  --     hit () () `shouldSatisfy` isNothing
  --     hit () () `shouldSatisfy` isNothing
