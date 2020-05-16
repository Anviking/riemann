module Harmony.NegativeSpec where

import           Harmony.Negative
import           Test.Hspec
import           Triad

spec :: Spec
spec =
  describe "Negative Triads" $
    golden (Major 0) (Minor 5)

golden :: Triad -> Triad -> Spec
golden x x' = it ("Negative " ++ show x ++ " is " ++ show x') $
  (normalize <$> invertTriad 0 x) `shouldBe` Just x'
