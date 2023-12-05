module AoC2023.Day1Part1 (run, solve) where

import Data.Char (isNumber)
import Data.List.Split (splitOn)

solve :: [String] -> Int
solve lines = sum (map calibration lines)

calibration :: String -> Int
calibration word = read (primero : [ultimo])
  where
    primero = head numeros
    ultimo = last numeros
    numeros = filter isNumber word

run :: IO ()
run = do
  inputLines <- lines <$> readFile "src/AoC2023/Day1.input"
  print $ solve inputLines