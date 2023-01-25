module Grid
  (
  Grid(..)
  ,Cell(..)
  ,getRow
  ,getColumn
  ,getCell
  ) where

data Cell = FixedCell {value :: Int} deriving (Show)

data Grid = Grid { cells :: [[Cell]] } deriving (Show)

-- Get the row for a given (x,y) coordinate from a Sudoku, including that cell
getRow :: Grid -> (Int, Int) -> [Cell]
getRow sudoku (x, y) = cells sudoku !! y

-- Get the column for a given (x,y) coordinate from a Sudoku, excluding that cell
getColumn :: Grid -> (Int, Int) -> [Cell]
getColumn sudoku (x, y) = [row !! x | row <- cells sudoku]

-- Get the cell at the given (x,y) coordinate from a Sudoku
getCell :: Grid -> (Int, Int) -> Cell
getCell sudoku (x, y) = row !! x
  where row = cells sudoku !! y

-- Replace a given cell in a sudoku with a fixed cell
fixCell :: Grid -> (Int, Int) -> Int -> Grid
fixCell (Grid cells) (x, y) v = Grid (take y cells ++ (take x theRow ++ FixedCell v : drop (x+1) theRow) : drop (y+1) cells)
  where theRow = cells !! y

-- Convert a Grid into a list of cells and their coordinates
coordList :: Grid -> [(Cell, (Int, Int))]
coordList sudoku = [(getCell sudoku (x, y), (x, y)) | x <- [0..(w-1)], y <- [0..(h-1)]]
  where h = length $ cells sudoku
        w = length $ head (cells sudoku)
