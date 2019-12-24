module TestAgeRestriction (demo) where

import           AgeRestriction.Demo (canIRegister)

import           Test.Hspec

demo :: IO ()
demo = hspec $ do
  describe "Age restriction" $ do
    it "Expect registration denied" $ do
      answer <- canIRegister 2005 3 4
      answer `shouldBe` False

    it "Expect registration approved" $ do
      answer <- canIRegister 1994 3 12
      answer `shouldBe` True

    it "Given invalid date, expect denied" $ do
      answer <- canIRegister 1998 15 34
      answer `shouldBe` False
