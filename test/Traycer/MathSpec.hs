module Traycer.MathSpec
  ( spec
  ) where

import Test.Hspec
import Traycer.Math

spec :: Spec
spec = do
  describe "quadratic" $ do
    it "solves the following equations" $ do
      quadratic 0 0 0 `shouldBe` Just (0, 0)
