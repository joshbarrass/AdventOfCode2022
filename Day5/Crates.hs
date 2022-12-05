module Crates
  ( parseDiagram
  , extractDiagram
  ) where

import Data.Stack

-- return a list of Maybe Char from a line of input to add to the stacks
parseInputLine :: String -> [Maybe Char]
parseInputLine [] = []
parseInputLine (_:x:_:' ':xs)
  | x == ' ' = Nothing : parseInputLine xs
  | otherwise = Just x : parseInputLine xs
parseInputLine (a:x:b:xs) = parseInputLine (a:x:b:' ':xs)

addCrateToStacks :: [Stack Char] -> [Maybe Char] -> [Stack Char]
addCrateToStacks [] _ = []
addCrateToStacks _ [] = []
addCrateToStacks (x:xs) (Nothing:cs) = x : addCrateToStacks xs cs
addCrateToStacks (x:xs) (Just c:cs) = stackPush x c : addCrateToStacks xs cs

addCratesToStacks :: [Stack Char] -> [[Maybe Char]] -> [Stack Char]
addCratesToStacks [] _ = []
addCratesToStacks xs [] = xs
addCratesToStacks xs (y:ys) = addCratesToStacks (addCrateToStacks xs y) ys

parseDiagram :: [String] -> [Stack Char]
parseDiagram xs = let
  parsedLines = map parseInputLine sx
  stacks = [stackNew | _ <- [1..(length $ head parsedLines)]]
  in addCratesToStacks stacks parsedLines
  where sx = reverse xs

-- extracts the diagram from the input lines
extractDiagram :: [String] -> [String]
extractDiagram = takeWhile (\x -> head x /= ' ') 
