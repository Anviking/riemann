module Harmony.NegativeSpec where

import           Harmony.Negative
import           Test.Hspec
import           Triad

spec :: Spec
spec =
  describe "Negative Triads" $
    golden (read "D") (read "Gm")

golden :: Triad -> Triad -> Spec
golden x x' = it ("Negative " ++ show x ++ " is " ++ show x') $
  (normalize <$> invertTriad 0 x) `shouldBe` Just x'
