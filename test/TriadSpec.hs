{-# LANGUAGE ScopedTypeVariables #-}
module TriadSpec where

import           Test.Hspec
import           Test.QuickCheck                   hiding (classify)
import           Test.QuickCheck.Arbitrary.Generic


import           Triad

spec :: Spec
spec =
  describe "Triads" $ do
    it "classify . toNotes == id" $ property $ \t ->
      classify (toNotes t) === Just t

    it "(normalize t1 /= normalize t2) ==> (show t1) /= (show t2)" $
      property $ \(t1 :: Triad) (t2 :: Triad) ->
        (normalize t1 /= normalize t2) ==> (show t1 /= show t2)

instance Arbitrary Triad where
  arbitrary = genericArbitrary
  shrink = genericShrink
