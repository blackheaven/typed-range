{-# LANGUAGE Safe #-}

module Data.Range.RangeTree {-# DEPRECATED "Use \"Data.Range.Algebra\" instead" #-}
   ( evaluate
   , RangeTree(..)
   , RangeOperation(..)
   ) where

import Data.Range.Data
import qualified Data.Range.Algebra as Alg

toExpr :: RangeTree a -> Alg.RangeExpr [Range a]
toExpr (RangeLeaf a) = Alg.const a
toExpr (RangeNodeInvert a) = Alg.invert (toExpr a)
toExpr (RangeNode RangeUnion a b) = Alg.union (toExpr a) (toExpr b)
toExpr (RangeNode RangeIntersection a b) = Alg.intersection (toExpr a) (toExpr b)
toExpr (RangeNode RangeDifference a b) = Alg.difference (toExpr a) (toExpr b)

-- | Evaluates a Range Tree into the final set of ranges that it compresses down to. Use
-- this whenever you want to finally evaluate your constructed Range Tree.
evaluate :: (Ord a, Enum a) => RangeTree a -> [Range a]
evaluate = Alg.eval . toExpr
