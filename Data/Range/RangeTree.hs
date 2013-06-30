module Data.Range.RangeTree 
   ( evaluate
   ) where

-- TODO I want to come up with a really efficient wap of performing these calculations. To
-- that extent I want to be able to generate a full parse tree that can just be evaluated.
-- That way you only have to convert to the internal representation once and convert out
-- once.

import Data.Range.Data
import Data.Range.RangeInternal
import Data.Range.RangeTreeInternal

evaluate :: (Ord a, Enum a) => RangeTree a -> [Range a]
evaluate = exportRangeMerge . evaluateRangeTree 
