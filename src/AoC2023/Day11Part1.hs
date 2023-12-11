{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AoC2023.Day11Part1 (run, solve, galaxyPairs, combinations) where

import Data.List.Split (splitOn)

-- Parsing

solve :: [String] -> Int
solve lines = undefined

galaxyPairs :: [a] -> [(a, a)]
galaxyPairs galaxies = map toTuple2 $ combinations 2 galaxies

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = []
combinations _ [] = []
combinations n (x : xs) = map (x :) (combinations (n - 1) xs) ++ combinations n xs

toTuple2 :: [a] -> (a, a)
toTuple2 (x : y : _) = (x, y)

run :: IO ()
run = do
  inputLines <- lines <$> readFile "src/AoC2023/Day11.input"
  print $ solve inputLines