module Rucksack
  ( Rucksack(..)
  , findDuplicate
  , getItemPriority
  , findAllBadges
  ) where

data Rucksack = Rucksack { inventory :: [Char] } deriving Show

compartments :: Rucksack -> ([Char], [Char])
compartments (Rucksack xs) = (take compartSize xs, drop compartSize xs)
  where l = length xs
        compartSize = l `div` 2

findSharedInLists :: (Eq a) => [a] -> [a] -> Maybe a
findSharedInLists _ [] = Nothing
findSharedInLists [] _ = Nothing
findSharedInLists (x:xs) ys
  | x `elem` ys = Just x
  | otherwise = findSharedInLists xs ys

findAllSharedInLists :: (Eq a) => [a] -> [a] -> [a]
findAllSharedInLists _ [] = []
findAllSharedInLists [] _ = []
findAllSharedInLists (x:xs) ys
  | x `elem` ys = x : findAllSharedInLists xs ys
  | otherwise = findAllSharedInLists xs ys

findDuplicate :: Rucksack -> Char
findDuplicate = (maybe ' ' id) . uncurry findSharedInLists . compartments

itemPriority :: (Eq a) => [(a, Int)] -> a -> Maybe Int
itemPriority [] _ = Nothing
itemPriority (kv:kvs) x
  | x == fst kv = Just $ snd kv
  | otherwise = itemPriority kvs x

getItemPriority :: Char -> Int
getItemPriority = (maybe 0 id) . itemPriority (zip (['a'..'z']++['A'..'Z']) [1..])

findBadge :: Rucksack -> Rucksack -> Rucksack -> Char
findBadge (Rucksack rs) (Rucksack ss) (Rucksack ts) = (maybe ' ' id) $ findSharedInLists firstTwoShared ts
  where firstTwoShared = findAllSharedInLists rs ss

findAllBadges :: [Rucksack] -> [Char]
findAllBadges [] = []
findAllBadges (rs:ss:ts:rss) = findBadge rs ss ts : findAllBadges rss
