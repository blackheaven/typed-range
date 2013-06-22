-- This module contains every function that purely performs operations on spans.
module Data.Range.Spans where

import Data.List (sortBy, insertBy)
import Data.Ord (comparing)

import Data.Range.Util
   
-- Assume that both inputs are sorted spans
insertionSortSpans :: (Ord a) => [(a, a)] -> [(a, a)] -> [(a, a)]
insertionSortSpans = insertionSort (comparing fst)

insertSpan :: Ord a => (a, b) -> [(a, b)] -> [(a, b)]
insertSpan = insertBy (comparing fst)

sortSpans :: (Ord a) => [(a, a)] -> [(a, a)]
sortSpans = sortBy (comparing fst)

-- Assume that you are given a sorted list of spans
joinSpans :: (Ord a, Enum a) => [(a, a)] -> [(a, a)]
joinSpans (f@(a, b) : s@(x, y) : xs) = 
   if succ b == x
      then joinSpans $ (a, y) : xs
      else f : joinSpans (s : xs)
joinSpans xs = xs

-- Assume that you are given a sorted list of spans
unionSpans :: Ord a => [(a, a)] -> [(a, a)]
unionSpans (f@(a, b) : s@(x, y) : xs) = if isBetween x f 
   then unionSpans ((a, max b y) : xs)
   else f : unionSpans (s : xs)
unionSpans xs = xs

-- Assume that you are given a sorted and joined list of spans
invertSpans :: (Ord a, Enum a) => [(a, a)] -> [(a, a)]
invertSpans ((_, x) : s@(y, _) : xs) = (succ x, pred y) : invertSpans (s : xs)
invertSpans _ = []
