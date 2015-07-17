module Data.Range.Algebra.Predicate where

import Data.Range.Data
import Data.Range.Util (isBetween)
import Data.Range.Algebra.Internal

inRange :: (Ord a) => a -> Range a -> Bool
inRange value (SingletonRange a) = value == a
inRange value (SpanRange x y) = isBetween value (x, y)
inRange value (LowerBoundRange lower) = lower <= value
inRange value (UpperBoundRange upper) = value <= upper
inRange _ InfiniteRange = True

predicateAlgebra :: Algebra (RangeExprF (a -> Bool)) (a -> Bool)
predicateAlgebra (Const f) a = f a
predicateAlgebra (Invert f) a = not (f a)
predicateAlgebra (Union f g) a = f a || g a
predicateAlgebra (Intersection f g) a = f a && g a
predicateAlgebra (Difference f g) a = f a && not (g a)

inRanges :: (Ord a) => [Range a] -> a -> Bool
inRanges rs a = any (inRange a) rs
