-- | Internally the range library converts your ranges into an internal representation of
-- multiple ranges that I call a RangeMerge. When you do multiple unions and intersections
-- in a row converting to and from that data structure becomes extra work that is not
-- required. To amortize those costs away the RangeTree structure exists. You can specify
-- a tree of operations in advance and then evaluate them all at once. This is not only
-- useful for efficiency but for parsing too. Use RangeTree's whenever you wish to perform
-- multiple operations in a row and wish for it to be as efficient as possible.
module Data.Range.RangeTree 
   ( evaluate
   , RangeTree(..)
   , RangeOperation(..)
   ) where

import Data.Range.Data
import Data.Range.RangeInternal
import Data.Range.RangeTreeInternal

-- | Evaluates a Range Tree into the final set of ranges that it compresses down to. Use
-- this whenever you want to finally evaluate your constructed Range Tree.
evaluate :: (Ord a, Enum a) => RangeTree a -> [Range a]
evaluate = exportRangeMerge . evaluateRangeTree 
