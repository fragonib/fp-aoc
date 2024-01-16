{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AoC2023.Day11Part1 (run, solve) where

import Control.Arrow (Arrow (second))
import Control.Lens (Lens', over, view, _1, _2)
import Data.List (sort, sortBy, sortOn, span)
import Data.List.Split (splitOn)

type Space = [Galaxy]

type Galaxy = (Int, Int)

solve :: [String] -> Int
solve lines = sum $ galaxyDistances expandedSpace
  where
    expandedSpace = (expandSpaceCoord _1 . expandSpaceCoord _2) galaxies
    galaxies = parseGalaxies lines

galaxyDistances :: Space -> [Int]
galaxyDistances [] = []
galaxyDistances (firstGalaxy : restOfGalaxies) =
  map (distance firstGalaxy) restOfGalaxies ++ galaxyDistances restOfGalaxies

distance :: Galaxy -> Galaxy -> Int
distance (row1, col1) (row2, col2) =
  abs (col2 - col1) + abs (row2 - row1)

expandSpaceCoord :: Lens' Galaxy Int -> Space -> Space
expandSpaceCoord galaxyCoordLens space =
  go galaxyCoordLens 0 [1 ..] sortedGalaxiesByCoord
  where
    sortedGalaxiesByCoord = sortOn (view galaxyCoordLens) space

go :: Lens' Galaxy Int -> Int -> [Int] -> Space -> Space
go _ _ _ [] = []
go galaxyLens shift (coord : coords) galaxies@(firstGalaxy : _)
  | coord /= view galaxyLens firstGalaxy =
      go galaxyLens (shift + 100 - 1) coords galaxies
  | otherwise =
      let (inC, rest) = span ((== coord) . view galaxyLens) galaxies
       in map (over galaxyLens (+ shift)) inC ++ go galaxyLens shift coords rest

-- Parsing

parseGalaxies :: [String] -> Space
parseGalaxies lines = concatMap parseRow $ zip [1 ..] lines

parseRow :: (Int, String) -> [Galaxy]
parseRow (rowNumber, line) = map addRow galaxiesAndCols
  where
    addRow (colNumber, _) = (rowNumber, colNumber)
    galaxiesAndCols = filter ((== '#') . snd) (zip [1 ..] line)

run :: IO ()
run = do
  inputLines <- lines <$> readFile "src/AoC2023/Day11.input"
  print $ solve inputLines