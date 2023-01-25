import Elf
import System.Environment
import Data.List

testInventory :: String
testInventory = "1000\n\
\2000\n\
\3000\n\
\\n\
\4000\n\
\\n\
\5000\n\
\6000\n\
\\n\
\7000\n\
\8000\n\
\9000\n\
\\n\
\10000\n"

-- parseInventory converts a string inventory into a proper inventory
parseInventory :: [String] -> [Int]
parseInventory = map read

-- take the lines of an input and split it into each inventory
getInventories :: [String] -> [[String]]
getInventories [] = []
getInventories xs = inv : if null remain then [] else getInventories (tail remain)
  where inv = takeWhile (not . null) xs
        remain = dropWhile (not . null) xs

-- parses lines of an input into elves
parseInventories :: [String] -> [Elf]
parseInventories = map (Elf . parseInventory) . getInventories

-- parses the entire input into elves
parseInput :: String -> [Elf]
parseInput = parseInventories . lines

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  input <- readFile filename
  let elves = parseInput input
  let sortedTotals = sortBy (flip compare) $ map totalCalories elves
  let totalTopThree = sum (take 3 sortedTotals)
  putStrLn $ "Total of top three calories: " ++ (show totalTopThree)
