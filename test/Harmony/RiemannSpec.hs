module Harmony.RiemannSpec where

import           Harmony.Riemann
import           Test.Hspec
import           Test.QuickCheck
import           Triad

spec :: Spec
spec =
  describe "Neo-riemann transformations" $ do
    it "p Dm = D" $
      normalize (p (Minor 0)) `shouldBe` Major 0
    it "h Dm = F#" $
      normalize (h (Minor 0)) `shouldBe` Major 4
    it "rountrips" $ property $ \t -> do
      let r = Minor 0
      normalize (f r (g r t)) === normalize t

-- TODO: Generate diminished chords
instance Arbitrary Triad where
  arbitrary = oneof
    [ Minor <$> arbitrary
    , Major <$> arbitrary
    ]




