module Data.Range.RangeTree where

-- TODO I want to come up with a really efficient wap of performing these calculations. To
-- that extent I want to be able to generate a full parse tree that can just be evaluated.
-- That way you only have to convert to the internal representation once and convert out
-- once.

import Data.Range.Range
import Data.Range.RangeInternal

data RangeOperation = RangeUnion | RangeIntersection

data RangeTree a 
   = RangeNode RangeOperation (RangeTree a) (RangeTree a)
   | RangeNodeInvert (RangeTree a)
   | RangeLeaf [Range a]

evaluate :: (Ord a, Enum a) => RangeTree a -> [Range a]
evaluate = exportRangeMerge . evaluateRangeTree 

-- TODO for the sake of testing this should be moved into its own module.
evaluateRangeTree :: (Ord a, Enum a) => RangeTree a -> RangeMerge a
evaluateRangeTree (RangeNode operation left right) = case operation of
   RangeUnion -> leftEval `unionRangeMerges` rightEval
   RangeIntersection -> leftEval `intersectionRangeMerges` rightEval
   where
      leftEval = evaluateRangeTree left 
      rightEval = evaluateRangeTree right
evaluateRangeTree (RangeNodeInvert node) = invertRM . evaluateRangeTree $ node
evaluateRangeTree (RangeLeaf ranges) = loadRanges ranges
