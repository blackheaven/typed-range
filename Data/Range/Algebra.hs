{-# LANGUAGE Safe #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Data.Range.Algebra
  ( RangeExpr
    -- ** Operations
  , const, invert, union, intersection, difference
    -- ** Evaluation
  , Algebra, RangeAlgebra(..)
  ) where

import Prelude hiding (const)

import Data.Range.Data
import Data.Range.Algebra.Internal
import Data.Range.Algebra.Range
import Data.Range.Algebra.Predicate

const :: a -> RangeExpr a
const = RangeExpr . Fix . Const

invert :: RangeExpr a -> RangeExpr a
invert = RangeExpr . Fix . Invert . getFix

union :: RangeExpr a -> RangeExpr a -> RangeExpr a
union a b = RangeExpr . Fix $ Union (getFix a) (getFix b)

intersection :: RangeExpr a -> RangeExpr a -> RangeExpr a
intersection a b = RangeExpr . Fix $ Intersection (getFix a) (getFix b)

difference :: RangeExpr a -> RangeExpr a -> RangeExpr a
difference a b = RangeExpr . Fix $ Difference (getFix a) (getFix b)

class RangeAlgebra a where
  eval :: Algebra RangeExpr a

instance (Ord a, Enum a) => RangeAlgebra [Range a] where
  eval = cata rangeAlgebra . getFix

instance RangeAlgebra (a -> Bool) where
  eval = cata predicateAlgebra . getFix
