module Data.Range.Algebra.Range where

import Data.Range.Data
import Data.Range.RangeInternal (exportRangeMerge)
import Data.Range.Algebra.Internal

rangeAlgebra :: (Ord a, Enum a) => Algebra (RangeExprF [Range a]) [Range a]
rangeAlgebra = exportRangeMerge . cata rangeMergeAlgebra . Fix . fmap (Fix . Const)
