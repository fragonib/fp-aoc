module AoC2023.Day11Spec (spec) where

import qualified AoC2023.Day11Part1 as Part1
import Test.Hspec

spec :: Spec
spec = do
  describe "Day11" $ do
    let input =
          [ "...#......",
            ".......#..",
            "#.........",
            "..........",
            "......#...",
            ".#........",
            ".........#",
            "..........",
            ".......#..",
            "#...#....."
          ]
    it "Part1" $ do
      Part1.solve input `shouldBe` 8410