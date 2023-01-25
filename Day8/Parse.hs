module Parse
  (
    parseGrid
  ) where 

import Grid
import Data.Char

parseSudokuLine :: String -> [Cell]
parseSudokuLine [] = []
parseSudokuLine (x:xs)
  | isDigit x = FixedCell (read [x]) : parseSudokuLine xs
  | otherwise = parseSudokuLine xs

-- Parses a Sudoku written out in text format. Only numbers, full
-- stops and line breaks will be parsed. Full stops represent empty
-- cells. Other characters will be ignored to allow structuring the
-- text however you please.
parseGrid :: String -> Grid
parseGrid s = let
  rows = lines s
  cells = filter ( not . null ) $ map parseSudokuLine rows
  in Grid cells
