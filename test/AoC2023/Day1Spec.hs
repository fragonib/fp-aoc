module AoC2023.Day1Spec (spec) where

import AoC2023.Day1Part1 (solve)
import Test.Hspec

spec :: Spec
spec = do
  describe "Day1" $ do
    it "Part1" $ do
      solve
        [ "1abc2",
          "pqr3stu8vwx",
          "a1b2c3d4e5f",
          "treb7uchet"
        ]
        `shouldBe` 142