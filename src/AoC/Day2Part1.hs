module AoC.Day2Part1 (run, solve) where

data Move = Rock | Paper | Scissors deriving (Show, Eq)

data Winner = Opponent | You | Draw deriving (Show, Eq)

type Play = (Move, Move)

-- Define here the functions you need to solve the puzzle
solve :: [String] -> Int
solve lines = undefined

-- This is the main function that read input lines and print the result
-- You can change it if you want, but it's just boilerplate (not required)
inputFile :: String
inputFile = "src/AoC/Day2.input"

run :: IO ()
run = do
  inputLines <- lines <$> readFile inputFile
  print $ solve inputLines