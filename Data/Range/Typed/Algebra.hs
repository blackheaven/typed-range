{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internally the range library converts your ranges into an internal
-- efficient representation of multiple ranges. When you do multiple unions and
-- intersections in a row converting to and from that data structure becomes
-- extra work that is not required. To amortize those costs away the @RangeExpr@
-- algebra exists. You can specify a tree of operations in advance and then
-- evaluate them all at once. This is not only useful for efficiency but for
-- parsing too. Build up @RangeExpr@'s whenever you wish to perform multiple
-- operations in a row, and evaluate it in one step to be as efficient as possible.
--
-- __Note:__ This module is based on F-Algebras to do much of the heavy conceptual
-- lifting. If you have never seen F-Algebras before then I highly recommend reading
-- through <https://www.schoolofhaskell.com/user/bartosz/understanding-algebras this introductory content>
-- from the School of Haskell.
--
-- == Examples
--
-- A simple example of using this module would look like this:
--
-- >>> import qualified Data.Range.Algebra as A
-- (A.eval . A.invert $ A.const [anyRange $ singleton 5]) :: [AnyRange Integer]
-- [LowerBoundRange 6,UpperBoundRange 4]
-- (0.01 secs, 597,656 bytes)
--
-- You can also use this module to evaluate range predicates.
module Data.Range.Typed.Algebra
  ( RangeExpr,

    -- ** Operations
    const,
    invert,
    union,
    intersection,
    difference,

    -- ** Evaluation
    Algebra,
    RangeAlgebra (..),
  )
where

import Control.Monad.Free
import Data.Range.Typed.Algebra.Internal
import Data.Range.Typed.Algebra.Predicate
import Data.Range.Typed.Algebra.Range
import Data.Range.Typed.Data
import Prelude hiding (const)

-- | Lifts the input value as a constant into an expression.
const :: a -> RangeExpr a
const = RangeExpr . Pure

-- | Returns an expression that represents the inverse of the input expression.
invert :: RangeExpr a -> RangeExpr a
invert = RangeExpr . Free . Invert . getFree

-- | Returns an expression that represents the set union of the input expressions.
union :: RangeExpr a -> RangeExpr a -> RangeExpr a
union a b = RangeExpr . Free $ Union (getFree a) (getFree b)

-- | Returns an expression that represents the set intersection of the input expressions.
intersection :: RangeExpr a -> RangeExpr a -> RangeExpr a
intersection a b = RangeExpr . Free $ Intersection (getFree a) (getFree b)

-- | Returns an expression that represents the set difference of the input expressions.
difference :: RangeExpr a -> RangeExpr a -> RangeExpr a
difference a b = RangeExpr . Free $ Difference (getFree a) (getFree b)

-- | Represents the fact that there exists an algebra for the given representation
-- of a range, so that a range expression of the same type can be evaluated, yielding
-- that representation.
class RangeAlgebra a where
  -- | This function is used to convert your built expressions into ranges.
  eval :: Algebra RangeExpr a

-- | Multiple ranges represented by a list of disjoint ranges.
-- Note that input ranges are allowed to overlap, but the output
-- ranges are guaranteed to be disjoint.
instance (Ord a) => RangeAlgebra [AnyRange a] where
  eval = iter rangeAlgebra . getFree

-- | Multiple ranges represented by a predicate function, indicating membership
-- of a point in one of the ranges.
instance RangeAlgebra (a -> Bool) where
  eval = iter predicateAlgebra . getFree
