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

-- pop N items from the stack and return the new stack and a list
popN :: Int -> Stack a -> (Stack a, [a])
popN 0 s = (s, [])
popN n s = (s'', i:is)
  where Just (s', i) = stackPop s
        (s'', is) = popN (n-1) s'

-- push all items onto a stack
pushAll :: [a] -> Stack a -> Stack a
pushAll [] s = s
pushAll (x:xs) s = pushAll xs $ stackPush s x

-- move N items from one stack onto the other, preserving order
moveN :: Int -> (Stack a, Stack a) -> (Stack a, Stack a)
moveN n (s, t) = (s', t')
  where (s', is) = popN n s
        t' = pushAll (reverse is) t

replace :: Int -> a -> [a] -> [a]
replace n x' xs = take n xs ++ x' : drop (n+1) xs

executeMove :: [Stack Char] -> Move -> [Stack Char]
executeMove xs m = let
  s = xs !! si
  d = xs !! di
  (s', d') = moveN (count m) (s, d)
  in replace di d' (replace si s' xs)
  where si = src m - 1
        di = dst m - 1

executeMoves :: [Stack Char] -> [Move] -> [Stack Char]
executeMoves xs [] = xs
executeMoves xs (m:ms) = executeMoves (executeMove xs m) ms
