module Interval
  ( Interval(..)
  , isSubset
  ) where

data Interval a = ClosedInterval { lower :: a, upper :: a }

-- determines whether the first interval is a subset of the second
isSubset :: (Ord a) => Interval a -> Interval a -> Bool
isSubset (ClosedInterval a c) (ClosedInterval b d)
  | ((a >= b) && (a <= d)) && ((c <= d) && (c >= b)) = True
  | otherwise = False
