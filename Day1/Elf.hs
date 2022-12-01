module Elf
  ( Elf(..)
  , totalCalories
  ) where

data Elf = Elf { inventory :: [Int] } deriving Show

totalCalories :: Elf -> Int
totalCalories = sum . inventory
