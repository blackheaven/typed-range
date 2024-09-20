module Data.Range.Typed.Algebra.Range where

import Control.Monad.Free
import Data.Range.Typed.Algebra.Internal
import Data.Range.Typed.Data
import Data.Range.Typed.RangeInternal (exportRangeMerge, loadRanges)

rangeAlgebra :: (Ord a) => Algebra RangeExprF [Range a]
rangeAlgebra = exportRangeMerge . iter rangeMergeAlgebra . Free . fmap (Pure . loadRanges)
