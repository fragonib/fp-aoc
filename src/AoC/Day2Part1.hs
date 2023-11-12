module AoC.Day2Part1 (run, solve) where

-- Some types that can be useful, you can use it if you want
data Move = Rock | Paper | Scissors deriving (Show, Eq)

data Winner = Opponent | You | Draw deriving (Show, Eq)

type Play = (Move, Move)

-- Define here the functions you need to solve the puzzle
solve :: [String] -> Int
solve lines = sum $ map (outcomePoints . parsePlay) lines

parsePlay :: String -> Play
parsePlay line =
  case words line of
    [first, second] -> (parseMove first, parseMove second)

parseMove :: String -> Move
parseMove "A" = Rock
parseMove "B" = Paper
parseMove "C" = Scissors
parseMove "X" = Rock
parseMove "Y" = Paper
parseMove "Z" = Scissors

outcomePoints :: Play -> Int
outcomePoints match@(opponent, you) =
  shapePoints you + winnerPoints match

winnerPoints :: Play -> Int
winnerPoints match =
  case playWinner match of
    Opponent -> 0
    You -> 6
    Draw -> 3

playWinner :: Play -> Winner
playWinner (Rock, Paper) = You
playWinner (Paper, Scissors) = You
playWinner (Scissors, Rock) = You
playWinner (opponent, you) | opponent == you = Draw
playWinner _ = Opponent

shapePoints :: Move -> Int
shapePoints Rock = 1
shapePoints Paper = 2
shapePoints Scissors = 3

-- This is the main function that read input lines and print the result
-- You can change it if you want, but it's just boilerplate (not required)
inputFile :: String
inputFile = "src/AoC/Day2.input"

run :: IO ()
run = do
  inputLines <- lines <$> readFile inputFile
  print $ solve inputLines