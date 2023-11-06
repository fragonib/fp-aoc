module AoC.Day1Part1 (solve) where

import Data.List.Split (splitOn)

-- Define here the functions you need to solve the puzzle
groupLinesByElf :: [String] -> [[String]]
groupLinesByElf lines = splitOn [""] lines

-- Solve will receive a list of strings, each string is a line of the input
solve :: [String] -> Int
solve lines = maxC numberedCalories
  where
    numberedCalories = zip [1 ..] summedCalories
    summedCalories = map sum (elvesCalories lines)

maxC :: [(Int, Int)] -> Int
maxC [(elfNumber, elfCalories)] = elfNumber
maxC (first : second : rest) =
  if snd first < snd second
    then maxC (second : rest)
    else maxC (first : rest)

-- Split the input lines into groups of lines by empty line
elvesCalories :: [String] -> [[Int]]
elvesCalories lines = map toCalories (groupLinesByElf lines)

-- Convert a list of lines into a list of integers
toCalories :: [String] -> [Int]
toCalories lines = map read lines

-- This is the main function that read input lines and print the result
-- You can change it if you want, but it is not required
main :: IO ()
main = do
  inputLines <- lines <$> getContents
  print $ solve inputLines