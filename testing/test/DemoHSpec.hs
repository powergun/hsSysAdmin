module DemoHSpec (demo) where

import Test.Hspec

simpleMathFunction a b c = (a * b) - c

simpleMathSpec :: Spec
simpleMathSpec = describe "Tests of our simple math function" $ do
  context "when the numbers are small" $
    it "Should match the our expected value" $
      simpleMathFunction 3 4 5 `shouldBe` 7
  context "when the numbers are big" $
    it "Should match the our expected value" $
      simpleMathFunction 22 12 64 `shouldBe` 200

demo :: IO ()
demo = hspec simpleMathSpec
