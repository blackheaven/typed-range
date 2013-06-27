module Data.Range.Range (
      Range(..),
      inRange,
      rangesOverlap,
      mergeRanges,
      invert,
      union,
      intersection,
      fromRanges,
      fromMergedRanges
   ) where

import Data.Range.Data
import Data.Range.RangeInternal
import Data.Range.Util

union :: (Ord a, Enum a) => [Range a] -> [Range a] -> [Range a]
union a b = exportRangeMerge $ unionRangeMerges (loadRanges a) (loadRanges b)

intersection :: (Ord a, Enum a) => [Range a] -> [Range a] -> [Range a]
intersection a b = exportRangeMerge $ intersectionRangeMerges (loadRanges a) (loadRanges b)

invert :: (Ord a, Enum a) => [Range a] -> [Range a]
invert = exportRangeMerge . invertRM . loadRanges

rangesOverlap :: (Ord a) => Range a -> Range a -> Bool
rangesOverlap (SingletonRange a) (SingletonRange b) = a == b
rangesOverlap (SingletonRange a) (SpanRange x y) = isBetween a (x, y)
rangesOverlap (SingletonRange a) (LowerBoundRange lower) = lower <= a
rangesOverlap (SingletonRange a) (UpperBoundRange upper) = a <= upper
rangesOverlap (SpanRange x y) (SpanRange a b) = isBetween x (a, b) || isBetween a (x, y)
rangesOverlap (SpanRange _ y) (LowerBoundRange lower) = lower <= y
rangesOverlap (SpanRange x _) (UpperBoundRange upper) = x <= upper
rangesOverlap (LowerBoundRange _) (LowerBoundRange _) = True
rangesOverlap (LowerBoundRange x) (UpperBoundRange y) = x <= y
rangesOverlap (UpperBoundRange _) (UpperBoundRange _) = True
rangesOverlap InfiniteRange _ = True
rangesOverlap a b = rangesOverlap b a

inRange :: (Ord a) => Range a -> a -> Bool
inRange (SingletonRange a) value = value == a
inRange (SpanRange x y) value = isBetween value (x, y)
inRange (LowerBoundRange lower) value = lower <= value
inRange (UpperBoundRange upper) value = value <= upper
inRange InfiniteRange _ = True

mergeRanges :: (Ord a, Enum a) => [Range a] -> [Range a]
mergeRanges = exportRangeMerge . loadRanges

fromRanges :: (Ord a, Enum a) => [Range a] -> [a]
fromRanges = concatMap fromRange
   where 
      fromRange range = case range of 
         SingletonRange x -> [x] 
         SpanRange a b -> [a..b]
         LowerBoundRange x -> iterate succ x
         UpperBoundRange x -> iterate pred x
         InfiniteRange -> zero : takeEvenly (tail $ iterate succ zero) (tail $ iterate pred zero)
            where
               zero = toEnum 0

fromMergedRanges :: (Ord a, Enum a) => [Range a] -> [a]
fromMergedRanges = fromRanges . mergeRanges
