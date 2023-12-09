module AoC2022.Day1Part2 (run, solve) where

import Data.List (sort)
import Data.List.Split (splitOn)

solve :: [String] -> Int
solve lines = sum (take 3 sortedElfDesc)
  where
    sortedElfDesc = reverse (sort summedElfCalories)
    summedElfCalories = map sum (elvesCalories lines)

groupLinesByElf :: [String] -> [[String]]
groupLinesByElf = splitOn [""]

elvesCalories :: [String] -> [[Int]]
elvesCalories lines = map toCalories groupedLines
  where
    groupedLines = groupLinesByElf lines

toCalories :: [String] -> [Int]
toCalories = map read

-- This is the main function that read input lines and print the result
-- You can change it if you want, but it is not required
inputFile :: String
inputFile = "src/AoC/Day1.input"

run :: IO ()
run = do
  inputLines <- lines <$> readFile inputFile
  print $ solve inputLines