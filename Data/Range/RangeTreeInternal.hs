module Data.Range.RangeTreeInternal where

import Data.Range.Data
import Data.Range.RangeInternal

evaluateRangeTree :: (Ord a, Enum a) => RangeTree a -> RangeMerge a
evaluateRangeTree (RangeNode operation left right) = case operation of
   RangeUnion -> leftEval `unionRangeMerges` rightEval
   RangeIntersection -> leftEval `intersectionRangeMerges` rightEval
   RangeDifference -> leftEval `intersectionRangeMerges` (invertRM rightEval)
   where
      leftEval = evaluateRangeTree left 
      rightEval = evaluateRangeTree right
evaluateRangeTree (RangeNodeInvert node) = invertRM . evaluateRangeTree $ node
evaluateRangeTree (RangeLeaf ranges) = loadRanges ranges
