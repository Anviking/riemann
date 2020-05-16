module TriadSpec where

import           Test.Hspec
import           Test.QuickCheck                   hiding (classify)
import           Test.QuickCheck.Arbitrary.Generic


import           Triad                             (Triad (..), classify,
                                                    toNotes)

spec :: Spec
spec =
  describe "Triads" $
    it "classify . toNotes == id" $ property $ \t ->
      classify (toNotes t) === Just t

instance Arbitrary Triad where
  arbitrary = genericArbitrary
  shrink = genericShrink
