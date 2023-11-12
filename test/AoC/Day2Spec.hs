module AoC.Day2Spec (spec) where

import AoC.Day2Part1 (solve)
import Test.Hspec

spec :: Spec
spec = do
  describe "Day2" $ do
    it "Part1" $ do
      solve
        [ "A Y",
          "B X",
          "C Z"
        ]
        `shouldBe` 15