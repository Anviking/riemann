module Harmony.RiemannSpec where

import           Harmony.Riemann
import           Test.Hspec
import           Test.QuickCheck
import           Triad
import Control.Monad (forM_)

import qualified Harmony.Axis    as Axis

spec :: Spec
spec = do
  describe "Neo-riemann transformations" $ do
    it "p Dm = D" $
      normalize (p (read "Dm")) `shouldBe` read "D"
    it "h Dm = F#" $
      normalize (h (read "Dm")) `shouldBe` read "F#"
    it "rountrips" $ property $ \t -> do
      let r = Minor 0
      normalize (f r (g r t)) === normalize t

  describe "Swapping axis" $ do
    let prop_preservesFunction f = property $ \chords -> do
        -- TODO: write coarbitrary for the transformation function
        let key = 0
        let function triad = Axis.classify key (root triad)
        let res = ((transform' (Minor key) f) chords)
        counterexample ("Result: " <> show res) $
          map function res === map function chords

    let ops =
          [ ("Swap1", swap1)
          , ("Swap2", swap2)
          , ("Rotate1", rotate1)
          , ("Rotate2", rotate2)
          , ("Weird1", weird1)
          , ("Weird2", weird2)
          , ("Weird3", weird3)
          ]

    forM_ ops $ \(desc, f) -> it (desc ++ " preserves function") $ prop_preservesFunction f


-- TODO: Generate diminished chords
instance Arbitrary Triad where
  arbitrary = oneof
    [ Minor <$> arbitrary
    , Major <$> arbitrary
    ]
  shrink t = shrinkType t ++ shrinkRoot t
    where
      shrinkRoot t = if root t /= 0 then [transposeTriad (const 0) t] else []

      shrinkType (Minor r) = []
      shrinkType (Major r) = [Minor r]
      shrinkType (Dim r) = [Minor r]




