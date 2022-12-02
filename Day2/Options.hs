module Options
  ( Option(..)
  , parseOption
  , playGame
  ) where

data Option = Rock | Paper | Scissors deriving (Show, Eq)

parseOption :: Char -> Option
parseOption 'A' = Rock
parseOption 'B' = Paper
parseOption 'C' = Scissors
parseOption 'X' = Rock
parseOption 'Y' = Paper
parseOption 'Z' = Scissors

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

-- takes an option and returns the points
playGame :: Option -> Option -> Int
playGame a b
  | optionCompare a b == GT = 0 + os
  | optionCompare a b == EQ = 3 + os
  | optionCompare a b == LT = 6 + os
  where os = optionScore b
