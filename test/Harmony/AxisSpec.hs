{-# LANGUAGE ScopedTypeVariables #-}
module Harmony.AxisSpec where

import           Test.Hspec
import           Test.QuickCheck                   hiding (classify)
import           Test.QuickCheck.Arbitrary.Generic


import           Triad (Triad, root)
import Harmony.Axis (classify, Function (..))

spec :: Spec
spec =
  describe "Axis Harmony" $ do
    golden "Dm Gm A" [T, S, D]
    golden "Dm E Eb" [T, S, D]
  where
    golden desc expected =
      it (desc ++ " -> " ++ show expected) $
        map (classify 0 . root . read) (words desc) `shouldBe` expected

instance Arbitrary Triad where
  arbitrary = genericArbitrary
  shrink = genericShrink
