module AoC.Day1Part2Spec (spec) where

import AoC.Day1Part2 (solve)
import Test.Hspec

spec :: Spec
spec = do
  describe "Day1" $ do
    it "Part2" $ do
      solve
        [ "1000",
          "2000",
          "3000",
          "",
          "4000",
          "",
          "5000",
          "6000",
          "",
          "7000",
          "8000",
          "9000",
          "",
          "10000"
        ]
        `shouldBe` 45000