{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module Data.Range.Algebra.Internal where

import Prelude hiding (const)

import Data.Range.Data
import Data.Range.RangeInternal

data RangeExprF a r
  = Const a
  | Invert r
  | Union r r
  | Intersection r r
  | Difference r r
  deriving (Show, Eq, Ord, Functor)

mapConst :: (a -> b) -> RangeExprF a r -> RangeExprF b r
mapConst f (Const a) = Const (f a)
mapConst _ (Invert r) = Invert r
mapConst _ (Union a b) = Union a b
mapConst _ (Intersection a b) = Intersection a b
mapConst _ (Difference a b) = Difference a b

newtype Fix f = Fix { unFix :: f (Fix f) }

instance Show (f (Fix f)) => Show (Fix f) where
    show x = "(" ++ show (unFix x) ++ ")"

instance Eq (f (Fix f)) => Eq (Fix f) where
    a == b = unFix a == unFix b

instance Ord (f (Fix f)) => Ord (Fix f) where
    a `compare` b = unFix a `compare` unFix b

newtype RangeExpr a = RangeExpr { getFix :: Fix (RangeExprF a) }
  deriving (Show, Eq, Ord)

instance Functor RangeExpr where
  fmap f = RangeExpr . f' . getFix
    where f' = Fix . mapConst f . fmap f' . unFix

type Algebra f a = f a -> a

cata :: Functor f => Algebra f a -> Fix f -> a
cata f = f . fmap (cata f) . unFix

rangeMergeAlgebra :: (Ord a, Enum a) => Algebra (RangeExprF [Range a]) (RangeMerge a)
rangeMergeAlgebra (Const a) = loadRanges a
rangeMergeAlgebra (Invert a) = invertRM a
rangeMergeAlgebra (Union a b) = a `unionRangeMerges` b
rangeMergeAlgebra (Intersection a b) = a `intersectionRangeMerges` b
rangeMergeAlgebra (Difference a b) = a `intersectionRangeMerges` invertRM b
