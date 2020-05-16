module NotesSpec where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic


import qualified Notes                             as N

spec :: Spec
spec =
  describe "Triads" $
    it "classify . toNotes == id" $ property $ \t ->
      N.classify (N.toNotes t) === Just t

instance Arbitrary N.Triad where
  arbitrary = genericArbitrary
  shrink = genericShrink
