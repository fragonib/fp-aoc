{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AoC2023.Day5Part2 (run, solve, parseSeeds) where

import Data.Char (isNumber)
import Data.Function (on)
import Data.List (sortBy)
import Data.List.Split (splitOn)

data Mapping = Mapping
  { destinationRangeStart :: Int,
    souceRangeStart :: Int,
    rangeLenght :: Int
  }
  deriving (Show, Eq)

type Almanac = [[Mapping]]

solve :: [String] -> Int
solve lines = minimum locations
  where
    locations = map (location almanac) seeds
    seeds = parseSeeds $ head $ head sections
    almanac = map (sortBy byStartRange) (parseAlmanac $ drop 1 sections)
    sections = splitInSections lines

location :: Almanac -> Int -> Int
location almanac seed = foldl (flip infer) seed almanac

infer :: [Mapping] -> Int -> Int
infer mappings targetNum =
  let foundMapping = binaryMappingSearch isInMapping targetNum mappings
   in case foundMapping of
        Nothing -> targetNum
        Just (Mapping destination source _) -> destination + (targetNum - source)

isInMapping :: Mapping -> Int -> Bool
isInMapping (Mapping destination source range) targetNum =
  targetNum >= source && targetNum < (source + range)

byStartRange :: Mapping -> Mapping -> Ordering
byStartRange = compare `on` souceRangeStart

binaryMappingSearch :: (Mapping -> Int -> Bool) -> Int -> [Mapping] -> Maybe Mapping
binaryMappingSearch _ _ [] = Nothing
binaryMappingSearch matcher elem mappings
  | matcher mapping elem = Just mapping
  | elem < source = binaryMappingSearch matcher elem smaller
  | otherwise = binaryMappingSearch matcher elem bigger
  where
    Mapping _ source _ = mapping
    (smaller, mapping : bigger) = splitAt index mappings
    index = length mappings `quot` 2

-- Parsing

splitInSections :: [String] -> [[String]]
splitInSections = splitOn [""]

parseSeeds :: String -> [Int]
parseSeeds line = seedsRange >>= expandSeedRange
  where
    seedsRange = window seeds
    seeds = (map read . drop 1 . words) line

expandSeedRange :: (Enum a, Num a) => (a, a) -> [a]
expandSeedRange (num, amount) = map (num +) [0 .. amount - 1]

window :: [a] -> [(a, a)]
window [] = []
window [a] = []
window [a, b] = [(a, b)]
window (a : b : r) = (a, b) : window r

parseAlmanac :: [[String]] -> Almanac
parseAlmanac = map parseMappings

parseMappings :: [String] -> [Mapping]
parseMappings lines = map mappingBuilder numbers
  where
    numbers = map (map read . words) mappings
    mappings = drop 1 lines

mappingBuilder :: [Int] -> Mapping
mappingBuilder [destination, source, length] =
  Mapping destination source length

-- Main

run :: IO ()
run = do
  inputLines <- lines <$> readFile "src/AoC2023/Day5.input"
  print $ solve inputLines