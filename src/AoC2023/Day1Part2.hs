module AoC2023.Day1Part2 (run, solve) where

import Data.Char (isAlpha, isNumber)
import Data.List (isSuffixOf)
import Debug.Trace (trace)

solve :: [String] -> Int
solve lines = sum (map evaluar lines)

evaluar :: String -> Int
evaluar word = calculation $ translateToNumbers initialParsed word
  where
    initialParsed = []

calculation :: String -> Int
calculation numbersLiteral = read [head numbersLiteral, last numbersLiteral]

translateToNumbers :: String -> String -> [Char]
translateToNumbers parsed [] = []
translateToNumbers parsed rest@(head : tail)
  | isNumber head = head : translateToNumbers parsed tail
  | otherwise =
      let newAddition = [head]
          toBeParsed = (parsed ++ newAddition)
       in case spellToNumber toBeParsed of
            Nothing -> translateToNumbers toBeParsed tail
            Just numberChar -> numberChar : translateToNumbers newAddition rest

spellToNumber :: String -> Maybe Char
spellToNumber spell
  | "one" `isSuffixOf` spell = Just '1'
  | "two" `isSuffixOf` spell = Just '2'
  | "three" `isSuffixOf` spell = Just '3'
  | "four" `isSuffixOf` spell = Just '4'
  | "five" `isSuffixOf` spell = Just '5'
  | "six" `isSuffixOf` spell = Just '6'
  | "seven" `isSuffixOf` spell = Just '7'
  | "eight" `isSuffixOf` spell = Just '8'
  | "nine" `isSuffixOf` spell = Just '9'
spellToNumber _ = Nothing

run :: IO ()
run = do
  inputLines <- lines <$> readFile "src/AoC2023/Day1.input"
  print $ solve inputLines