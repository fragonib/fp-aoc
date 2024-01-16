module AoC2023.Day19Part1 (run, solve) where

import Data.Char (isAlpha)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M

type Name = String

data Category = Extreme | Musical | Aerodynamic | Shiny deriving (Show, Eq)

data WorkFlow = WorkFlow
  { name :: Name,
    steps :: [Rule]
  }
  deriving (Show, Eq)

data Rule
  = LessThan Category Int Rule
  | GreaterThan Category Int Rule
  | Accept
  | JumpTo Name
  | Reject
  deriving (Show, Eq)

data Part = Part
  { extreme :: Int,
    musical :: Int,
    aerodynamic :: Int,
    shiny :: Int
  }
  deriving (Show, Eq)

type WorkflowIndex = Map Name WorkFlow

type Problem = (WorkflowIndex, [Part])

solve :: [String] -> Int
solve inputLines = solving (parseInput inputLines)

solving :: Problem -> Int
solving (workflows, parts) = sum $ map process parts
  where
    process part =
      let total = extreme part + musical part + aerodynamic part + shiny part
       in if acceptPart workflows part then total else 0

initialWorkflow :: Name
initialWorkflow = "in"

acceptPart :: WorkflowIndex -> Part -> Bool
acceptPart workflows part =
  evalPart part $ rulesFinder initialWorkflow
  where
    rulesFinder :: Name -> [Rule]
    rulesFinder name =
      case M.lookup name workflows of
        Nothing -> []
        Just (WorkFlow _ rules) -> rules

    evalPart :: Part -> [Rule] -> Bool
    evalPart _ [] = False
    evalPart _ (Accept : _) = True
    evalPart _ (Reject : _) = False
    evalPart _ (JumpTo workflowName : _) =
      evalPart part $ rulesFinder workflowName
    evalPart part (LessThan Extreme n rule : restOfRules)
      | extreme part < n = evalPart part [rule]
      | otherwise = evalPart part restOfRules
    evalPart part (LessThan Musical n rule : restOfRules)
      | musical part < n = evalPart part [rule]
      | otherwise = evalPart part restOfRules
    evalPart part (LessThan Aerodynamic n rule : restOfRules)
      | aerodynamic part < n = evalPart part [rule]
      | otherwise = evalPart part restOfRules
    evalPart part (LessThan Shiny n rule : restOfRules)
      | shiny part < n = evalPart part [rule]
      | otherwise = evalPart part restOfRules
    evalPart part (GreaterThan Extreme n rule : restOfRules)
      | extreme part > n = evalPart part [rule]
      | otherwise = evalPart part restOfRules
    evalPart part (GreaterThan Musical n rule : restOfRules)
      | musical part > n = evalPart part [rule]
      | otherwise = evalPart part restOfRules
    evalPart part (GreaterThan Aerodynamic n rule : restOfRules)
      | aerodynamic part > n = evalPart part [rule]
      | otherwise = evalPart part restOfRules
    evalPart part (GreaterThan Shiny n rule : restOfRules)
      | shiny part > n = evalPart part [rule]
      | otherwise = evalPart part restOfRules

-- Parsing

partitionBy :: (a -> Bool) -> [a] -> ([a], [a])
partitionBy = break

parseInput :: [String] -> Problem
parseInput lines = (workflows, parts)
  where
    (lsW, _ : lsP) = partitionBy (== "") lines
    workflows = foldr (insertW . parseWorkFlow) M.empty lsW
    parts = map parsePart lsP
    insertW w = M.insert (name w) w

parseWorkFlow :: String -> WorkFlow
parseWorkFlow s = WorkFlow name rules
  where
    (name, _ : sRules) = partitionBy (== '{') s
    rules = map parseRule $ splitOn "," $ init sRules

parseRule :: String -> Rule
parseRule "A" = Accept
parseRule "R" = Reject
parseRule s =
  let (name, rest) = span isAlpha s
      category "a" = Aerodynamic
      category "x" = Extreme
      category "m" = Musical
      category "s" = Shiny
      category _ = error "Unknown category"
   in case rest of
        '<' : numAndTarget ->
          let (num, _ : target) = partitionBy (== ':') numAndTarget
           in LessThan (category name) (read num) (parseRule target)
        '>' : numAndTarget ->
          let (num, _ : target) = partitionBy (== ':') numAndTarget
           in GreaterThan (category name) (read num) (parseRule target)
        _ -> JumpTo name

parsePart :: String -> Part
parsePart s = Part extreme musical aerodynamic shiny
  where
    [extreme, musical, aerodynamic, shiny] =
      map (read . drop 2) $ splitOn "," $ tail (init s)

run :: IO ()
run = do
  inputLines <- lines <$> readFile "src/AoC2023/Day19.input"
  print $ show $ solve inputLines
