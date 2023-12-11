module AoC2023.Day11Spec (spec) where

import qualified AoC2023.Day11Part1 as Part1
import Test.Hspec

spec :: Spec
spec = do
  describe "Day11" $ do
    let input =
          [ ".........2...",
            "3............",
            ".............",
            ".............",
            "........4....",
            ".5...........",
            "............6",
            ".............",
            ".............",
            ".........7...",
            "8....9......."
          ]
    it "Part1" $ do
      Part1.solve input `shouldBe` 374