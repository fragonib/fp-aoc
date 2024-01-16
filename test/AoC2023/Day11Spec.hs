module AoC2023.Day11Spec (spec) where

import qualified AoC2023.Day11Part1 as Part1
import Test.Hspec

spec :: Spec
spec = do
  describe "Day11" $ do
    let inputLines =
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
      Part1.solve inputLines `shouldBe` 8410