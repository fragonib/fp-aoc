module AoC.Day1Part1 (run, solve) where

import Data.List.Split (splitOn)

-- Define here the functions you need to solve the puzzle
groupLinesByElf :: [String] -> [[String]]
groupLinesByElf lines = splitOn [""] lines

-- Solve will receive a list of strings, each string is a line of the input
solve :: [String] -> Int
solve lines = maxCalories summedElfCalories
  where
    summedElfCalories = map' sum (elvesCalories lines)

-- Calculates maximum calories
maxCalories :: [Int] -> Int
maxCalories [elfCalories] = elfCalories
maxCalories (first : second : rest) =
  if first < second
    then maxCalories (second : rest)
    else maxCalories (first : rest)

-- Split the input lines into groups of lines by empty line
elvesCalories :: [String] -> [[Int]]
elvesCalories lines = map' toCalories (groupLinesByElf lines)

-- Convert a list of lines into a list of integers
toCalories :: [String] -> [Int]
toCalories lines = map' read lines

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

-- This is the main function that read input lines and print the result
-- You can change it if you want, but it is not required
inputFile :: String
inputFile = "src/AoC/Day1.input"

run :: IO ()
run = do
  inputLines <- lines <$> readFile inputFile
  print $ solve inputLines