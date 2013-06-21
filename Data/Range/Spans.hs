-- This module contains every function that purely performs operations on spans.
module Data.Range.Spans where

import Data.List (sortBy)
import Data.Ord (comparing)

import Data.Range.Util
   
insertionSortSpans :: (Ord a) => [(a, a)] -> [(a, a)] -> [(a, a)]
insertionSortSpans = insertionSort (comparing fst)

sortSpans :: (Ord a) => [(a, a)] -> [(a, a)]
sortSpans = sortBy (comparing fst)

joinSpans :: (Ord a, Enum a) => [(a, a)] -> [(a, a)]
joinSpans (f@(a, b) : s@(x, y) : xs) = 
   if succ b == x
      then joinSpans $ (a, y) : xs
      else f : joinSpans (s : xs)
joinSpans xs = xs

unionSpans :: Ord a => [(a, a)] -> [(a, a)]
unionSpans (f@(a, b) : s@(x, y) : xs) = if isBetween x f 
   then unionSpans ((a, max b y) : xs)
   else f : unionSpans (s : xs)
unionSpans xs = xs
