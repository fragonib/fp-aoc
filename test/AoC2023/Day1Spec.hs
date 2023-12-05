module AoC2023.Day1Spec (spec) where

import qualified AoC2023.Day1Part1 as Part1
import qualified AoC2023.Day1Part2 as Part2
import Test.Hspec

spec :: Spec
spec = do
  describe "Day1" $ do
    it "Part1" $ do
      Part1.solve
        [ "1abc2",
          "pqr3stu8vwx",
          "a1b2c3d4e5f",
          "treb7uchet"
        ]
        `shouldBe` 142

    it "Part2" $ do
      Part2.solve
        [ "two1nine",
          "eightwothree",
          "abcone2threexyz",
          "xtwone3four",
          "4nineeightseven2",
          "zoneight234",
          "7pqrstsixteen"
        ]
        `shouldBe` 281