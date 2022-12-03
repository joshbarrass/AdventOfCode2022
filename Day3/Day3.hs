import Rucksack
import System.Environment

parseInput :: String -> [Rucksack]
parseInput = map Rucksack . lines

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  input <- readFile filename
  let rucksacks = parseInput input
  let duplicates = map findDuplicate rucksacks
  let priorities = map getItemPriority duplicates
  let s = sum priorities
  putStrLn $ "Total of duplicates: " ++ show s
