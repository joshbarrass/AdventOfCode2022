import Interval
import System.Environment

eitherIsSubset :: (Ord a) => Interval a -> Interval a -> Bool
eitherIsSubset a b = isSubset a b || isSubset b a

-- split a list at a particular item, removing that item
-- if a list does not contain that item, return the original list and an empty list 
splitAtItem :: (Eq a) => a -> [a] -> ([a], [a])
splitAtItem x ys = (takeWhile (/=x) ys, if droppedEmpty then [] else tail dropped)
  where dropped = dropWhile (/=x) ys
        droppedEmpty = null dropped

-- return a closed interval based on the input
parseInterval :: String -> Interval Int
parseInterval xs = let
  (lower, upper) = splitAtItem '-' xs
  in ClosedInterval (read lower) (read upper)

parseIntervalPair :: String -> (Interval Int, Interval Int)
parseIntervalPair xs = let
  (interval1, interval2) = splitAtItem ',' xs
  in (parseInterval interval1, parseInterval interval2)

parseInput :: String -> [(Interval Int, Interval Int)]
parseInput = map parseIntervalPair . lines

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  input <- readFile filename
  let pairs = parseInput input
  let subsets = filter (uncurry eitherIsSubset) pairs
  let numSubsets = length subsets
  putStrLn $ "Number of fully contained ranges: " ++ (show numSubsets)
