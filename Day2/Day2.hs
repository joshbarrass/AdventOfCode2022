import Options
import System.Environment

testInput = "A Y\n\
\B X\n\
\C Z"

parseInput :: String -> [(Option, Option)]
parseInput xs = [(parseOption $ head l, parseOption $ head (drop 2 l)) | l <- lines xs]

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  input <- readFile filename
  let strategy = parseInput input
  let totalPoints = sum $ map (uncurry playGame) strategy
  putStrLn $ "Total Points: " ++ show totalPoints
