module AoC.Day2Spec (spec) where

import qualified AoC.Day2Part1 as Part1
import qualified AoC.Day2Part2 as Part2
import Test.Hspec

spec :: Spec
spec = do
  describe "Day2" $ do
    it "Part1" $ do
      Part1.solve
        [ "A Y",
          "B X",
          "C Z"
        ]
        `shouldBe` 15

    it "Part2" $ do
      Part2.solve
        [ "A Y",
          "B X",
          "C Z"
        ]
        `shouldBe` 12