module Moves
  ( extractMoves
  , parseMoves
  , executeMoves
  ) where

import Data.Stack

data Move = Move {count :: Int, src :: Int, dst :: Int} deriving Show

-- extracts the moves from the input lines
extractMoves :: [String] -> [String]
extractMoves = tail . dropWhile (not . null)

parseMove :: String -> Move
parseMove xs = let
  r1 = drop (length "move ") xs
  count = read $ takeWhile (/= ' ') r1
  r2 = drop (length " from ") $ dropWhile (/= ' ') r1
  src = read $ takeWhile (/= ' ') r2
  dst = read $ drop (length " to ") $ dropWhile (/= ' ') r2
  in Move count src dst

parseMoves :: [String] -> [Move]
parseMoves [] = []
parseMoves (x:xs) = parseMove x : parseMoves xs

-- pops the first item of the first stack onto the second stack
popOnto :: (Stack a, Stack a) -> (Stack a, Stack a)
popOnto (s, t) = (s', stackPush t i)
  where Just (s', i) = stackPop s

-- pop N items from the first stack to the second stack
popNOnto :: Int -> (Stack a, Stack a) -> (Stack a, Stack a)
popNOnto 0 ss = ss
popNOnto n ss = popNOnto (n-1) $ popOnto ss

replace :: Int -> a -> [a] -> [a]
replace n x' xs = take n xs ++ x' : drop (n+1) xs

executeMove :: [Stack Char] -> Move -> [Stack Char]
executeMove xs m = let
  s = xs !! si
  d = xs !! di
  (s', d') = popNOnto (count m) (s, d)
  in replace di d' (replace si s' xs)
  where si = src m - 1
        di = dst m - 1

executeMoves :: [Stack Char] -> [Move] -> [Stack Char]
executeMoves xs [] = xs
executeMoves xs (m:ms) = executeMoves (executeMove xs m) ms
