{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AoC2023.Day5Part2 (run, solve, parseSeeds) where

import Data.Char (isNumber)
import Data.Function (on)
import Data.List (sortBy)
import Data.List.Split (splitOn)

data Mapping = Mapping
  { mappingDestination :: Int,
    mappingSource :: Int,
    mappingLength :: Int
  }
  deriving (Show, Eq)

type Almanac = [[Mapping]]

data SeedRange = SeedRange
  { seedStart :: Int,
    seedLength :: Int
  }
  deriving (Show, Eq)

solve :: [String] -> Int
solve lines =
  let locationsOfOnleSeedStarts = map (location almanac . seedStart) splittedSeedRanges
   in minimum locationsOfOnleSeedStarts
  where
    splittedSeedRanges = splitSeedRanges almanac seedRanges
    (seedRanges, almanac) = parseProblemLines lines

splitSeedRanges :: Almanac -> [SeedRange] -> [SeedRange]
splitSeedRanges almanac = concatMap (splitSeedRange almanac)

splitSeedRange :: Almanac -> SeedRange -> [SeedRange]
splitSeedRange almanac seed@(SeedRange start length) =
  if maxRange == length
    then [seed]
    else SeedRange start maxRange : splitSeedRange almanac nextSeedRange
  where
    nextSeedRange = SeedRange (start + maxRange) (length - maxRange)
    maxRange = seedLength $ foldl (flip maxSeedRange) seed almanac

maxSeedRange :: [Mapping] -> SeedRange -> SeedRange
maxSeedRange mappings (SeedRange sStart sLength) =
  let foundMapping = mappingBinarySearch isInMapping sStart mappings
   in case foundMapping of
        Nothing -> SeedRange sStart maxRange
          where
            maxRange = case nextMapping of
              Nothing -> sLength
              Just mapping -> min sLength (mappingSource mapping - sStart)
            nextMapping = head' $ filter ((> sStart) . mappingSource) mappings
        Just (Mapping destination mSource mLength) ->
          SeedRange
            (applyMapping foundMapping sStart)
            (min sLength (mLength - (sStart - mSource)))

location :: Almanac -> Int -> Int
location almanac seed = foldl (flip lookMappingAndApply) seed almanac

lookMappingAndApply :: [Mapping] -> Int -> Int
lookMappingAndApply mappings targetNum = applyMapping foundMapping targetNum
  where
    foundMapping = mappingBinarySearch isInMapping targetNum mappings

applyMapping :: Maybe Mapping -> Int -> Int
applyMapping Nothing targetNum = targetNum
applyMapping (Just (Mapping destination source _)) targetNum =
  destination + (targetNum - source)

isInMapping :: Mapping -> Int -> Bool
isInMapping (Mapping destination source range) targetNum =
  targetNum >= source && targetNum < (source + range)

compareByStartRange :: Mapping -> Mapping -> Ordering
compareByStartRange = compare `on` mappingSource

mappingBinarySearch :: (Mapping -> Int -> Bool) -> Int -> [Mapping] -> Maybe Mapping
mappingBinarySearch _ _ [] = Nothing
mappingBinarySearch matcher elem mappings
  | matcher mapping elem = Just mapping
  | elem < source = mappingBinarySearch matcher elem smaller
  | otherwise = mappingBinarySearch matcher elem bigger
  where
    Mapping _ source _ = mapping
    (smaller, mapping : bigger) = splitAt index mappings
    index = length mappings `quot` 2

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : xs) = Just x

-- Parsing

parseProblemLines :: [String] -> ([SeedRange], Almanac)
parseProblemLines lines = (seedRanges, almanac)
  where
    seedRanges = parseSeeds $ head $ head sections
    almanac = map (sortBy compareByStartRange) (parseAlmanac $ drop 1 sections)
    sections = splitInSections lines

splitInSections :: [String] -> [[String]]
splitInSections = splitOn [""]

parseSeeds :: String -> [SeedRange]
parseSeeds line = map (uncurry SeedRange) seedsPairs
  where
    seedsPairs = window seeds
    seeds = (map read . drop 1 . words) line

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