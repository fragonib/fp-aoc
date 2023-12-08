{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Used otherwise as a pattern" #-}
module AoC2023.Day5Part1 (run, solve) where

import Data.Char (isNumber)
import Data.List.Split (splitOn)

data Mapping = Mapping
  { destinationRangeStart :: Int,
    souceRangeStart :: Int,
    rangeLenght :: Int
  }
  deriving (Show, Eq)

type Mappings = [Mapping]

type Almanac = [Mappings]

-- Parsing

solve :: [String] -> Int
solve lines = minimum $ map (location almanac) seeds
  where
    seeds = parseSeeds $ head $ head sections
    almanac = parseAlmanac $ drop 1 sections
    sections = splitLines lines

location :: Almanac -> Int -> Int
location almanac seed = foldl (flip infer) seed almanac

parseSeeds :: String -> [Int]
parseSeeds = map read . drop 1 . words

parseAlmanac :: [[String]] -> Almanac
parseAlmanac = map parseMappings

parseMappings :: [String] -> Mappings
parseMappings lines = map (\(x, y, z) -> Mapping x y z) numbers
  where
    numbers = map (tuplify3 . map read . words) mappings
    mappings = drop 1 lines

splitLines :: [String] -> [[String]]
splitLines = splitOn [""]

tuplify3 :: [a] -> (a, a, a)
tuplify3 [x, y, z] = (x, y, z)

infer :: Mappings -> Int -> Int
infer [] num = num
infer (rule : rest) num =
  case rule of
    Mapping destination source range
      | num >= source && num <= source + range -> destination + (num - source)
      | otherwise -> infer rest num

run :: IO ()
run = do
  inputLines <- lines <$> readFile "src/AoC2023/Day5.input"
  print $ solve inputLines