module AoC.Day1Spec (spec) where

import qualified AoC.Day1Part1 as Part1
import qualified AoC.Day1Part2 as Part2
import Test.Hspec

spec :: Spec
spec = do
  describe "Day1" $ do
    it "Part1" $ do
      Part1.solve
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
        `shouldBe` 24000

    it "Part2" $ do
      Part2.solve
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