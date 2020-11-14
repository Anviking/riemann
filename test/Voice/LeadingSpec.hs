{-# LANGUAGE ScopedTypeVariables #-}
module Voice.LeadingSpec where

import           Test.Hspec

import           Voice.Leading (Motion (..), classify)

spec :: Spec
spec =
  describe "Voice Leading" $
    describe "classify motions" $ do
      golden [0,0,1] Oblique
      golden [-1,0,1] Contrary
      golden [1,1,0] Similar
      golden [1,1,1] Similar
  where
    golden desc expected =
      it (show desc ++ " -> " ++ show expected) $
        classify desc `shouldBe` expected
