module Options
  ( Option(..)
  , WinState(..)
  , parseOption
  , parseWinState
  , playGame
  ) where

data Option = Rock | Paper | Scissors deriving (Show, Eq)
data WinState = Win | Draw | Lose deriving (Show, Eq)

parseOption :: Char -> Option
parseOption 'A' = Rock
parseOption 'B' = Paper
parseOption 'C' = Scissors

parseWinState :: Char -> WinState
parseWinState 'X' = Lose
parseWinState 'Y' = Draw
parseWinState 'Z' = Win

optionCompare :: Option -> Option -> Ordering
optionCompare Paper Rock = GT
optionCompare Scissors Paper = GT
optionCompare Rock Scissors = GT
optionCompare a b
  | a == b = EQ
  | otherwise = LT

optionScore :: Option -> Int
optionScore Rock = 1
optionScore Paper = 2
optionScore Scissors = 3

getOptionForWinState :: Option -> WinState -> Option
getOptionForWinState a Draw = a
getOptionForWinState Rock Win = Paper
getOptionForWinState Paper Win = Scissors
getOptionForWinState Scissors Win = Rock
-- because rock paper scissors is permutative 
getOptionForWinState a Lose = getOptionForWinState (getOptionForWinState a Win) Win

-- takes an option and returns the points
playGame :: Option -> WinState -> Int
playGame a b
  | optionCompare a playerMove == GT = 0 + os
  | optionCompare a playerMove == EQ = 3 + os
  | optionCompare a playerMove == LT = 6 + os
  where playerMove = getOptionForWinState a b
        os = optionScore playerMove
