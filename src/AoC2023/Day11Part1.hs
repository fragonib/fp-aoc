{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AoC2023.Day11Part1 (run, solve, expand) where

import Control.Arrow (Arrow (second))
import Control.Lens (Lens', over, view, _1, _2)
import Data.List (sort, sortBy, sortOn, span)
import Data.List.Split (splitOn)

type Space = [Galaxy]

type Galaxy = (Int, Int)

solve :: [String] -> Int
solve lines = sum $ allDistances expandedSpace
  where
    expandedSpace = expand _1 $ expand _2 galaxies
    galaxies = parsing lines

allDistances :: Space -> [Int]
allDistances [] = []
allDistances (g : gs) = map (distance g) gs ++ allDistances gs

distance :: Galaxy -> Galaxy -> Int
distance (r1, c1) (r2, c2) = abs (c2 - c1) + abs (r2 - r1)

expand :: Lens' Galaxy Int -> Space -> Space
expand l s = go 0 [1 ..] (sortOn (view l) s)
  where
    go _ _ [] = []
    go shift (coord : coords) gs@(g : _)
      | coord /= view l g = go (shift + 1000000 - 1) coords gs
      | otherwise =
          let (inC, rest) = span ((== coord) . view l) gs
           in map (over l (+ shift)) inC ++ go shift coords rest

-- Parsing

parsing :: [String] -> Space
parsing lines = concatMap parseRow $ zip [1 ..] lines

parseRow :: (Int, String) -> [Galaxy]
parseRow (rowNumber, line) = map addRow galaxiesAndCols
  where
    addRow (colNumber, _) = (rowNumber, colNumber)
    galaxiesAndCols = filter ((== '#') . snd) (zip [1 ..] line)

run :: IO ()
run = do
  inputLines <- lines <$> readFile "src/AoC2023/Day11.input"
  print $ solve inputLines