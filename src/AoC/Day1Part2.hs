module AoC.Day1Part2 (run, solve) where

import Data.List (sort)
import Data.List.Split (splitOn)

solve :: [String] -> Int
solve lines = sum (take 3 sortedElf)
  where
    sortedElf = reverse (sort summedElfCalories)
    summedElfCalories = map' sum (elvesCalories lines)

groupLinesByElf :: [String] -> [[String]]
groupLinesByElf = splitOn [""]

elvesCalories :: [String] -> [[Int]]
elvesCalories lines = map' toCalories (groupLinesByElf lines)

toCalories :: [String] -> [Int]
toCalories = map' read

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