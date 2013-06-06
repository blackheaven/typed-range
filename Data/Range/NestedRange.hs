module Data.Range.NestedRange where

import Data.Range.Range

data NestedRange a = NestedRange [Range a]


-- I wanted to know if a nested number of elements are in a given range. That way I can
-- just immediately run a single function and tell things about ranges.

inNestedRange :: Ord a => [a] -> NestedRange a -> Bool
inNestedRange values (NestedRange ranges) = go values ranges
   where
      go :: Ord a => [a] -> [Range a] -> Bool
      go [] [] = True -- If there is nothing left then they are equal
      go _  [] = True -- If you have already found the values you have to be in range then they are
      go [] _  = False -- If you have not fully matched it yet then it is not in range.
      go (value:values) (range:ranges) = inRange range value && go values ranges
