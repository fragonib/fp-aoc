module AoC2023.Day1Part1 (run, solve) where

import Data.Char (isNumber)
import Data.List.Split (splitOn)

-- Define here the functions you need to solve the puzzle
solve :: [String] -> Int
solve lines = sum (map calibration lines)

calibration :: String -> Int
calibration word = read (primero : [ultimo])
  where
    primero = head numeros
    ultimo = last numeros
    numeros = filter isNumber word

-- This is the main function that read input lines and print the result
-- You can change it if you want, but it's just boilerplate (not required)
inputFile :: String
inputFile = "src/AoC2023/Day1.input"

run :: IO ()
run = do
  inputLines <- lines <$> readFile inputFile
  print $ solve inputLines