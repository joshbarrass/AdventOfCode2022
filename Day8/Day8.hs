import Grid
import Parse
import System.Environment
import Debug.Trace

tail' :: [a] -> [a]
tail' [] = []
tail' xs = tail xs

-- determine whether the tree at a particular coordinate is visible
isVisible :: Grid -> (Int, Int) -> Bool
isVisible grid (x, y) = let
  c = (x, y)
  this = getCell grid c
  row = getRow grid c
  column = getColumn grid c

  north = take y column
  east = tail' $ drop x row
  south = tail' $ drop y column
  west = take x row

  fn = \x -> value x < (value this)
  
  in (all fn north) || (all fn east) || (all fn south) || (all fn west)

viewDistance :: Int -> [Int] -> Int
viewDistance _ [] = 0
viewDistance height (x:xs)
  | x >= height = 1
  | otherwise = 1 + viewDistance height xs

viewingScore :: Grid -> (Int, Int) -> Int
viewingScore grid (x, y) = let
  c = (x, y)
  this = getCell grid c
  row = getRow grid c
  column = getColumn grid c

  north = reverse $ take y column
  east = tail' $ drop x row
  south = tail' $ drop y column
  west = reverse $ take x row

  views = [north, east, south, west]
  viewDistances = map ((viewDistance (value this)) . map value) views
  in product viewDistances

countBool :: [Bool] -> Int
countBool [] = 0
countBool (True:xs) = 1 + countBool xs
countBool (False:xs) = countBool xs

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  input <- readFile filename
  let grid = parseGrid input
  let height = length (cells grid)
  let width = length (head $ cells grid)
  putStrLn $ "Puzzle is (" ++ show width ++ ", " ++ show height ++ ")"
  let allCoords = [(x,y) | x <- [0..(width-1)], y <- [0..(height-1)]]
  let visibility = map (isVisible grid) allCoords
  let nVisible = countBool visibility
  putStrLn $ "Number visible: " ++ (show nVisible)
  let viewingscores = map (viewingScore grid) allCoords
  putStrLn $ "Best viewing score: " ++ (show $ maximum viewingscores)
