{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Data.Range.Typed.Algebra.Internal where

import Control.Monad.Free
import Data.Functor.Classes
import Data.Range.Typed.RangeInternal
import Prelude hiding (const)

data RangeExprF r
  = Invert r
  | Union r r
  | Intersection r r
  | Difference r r
  deriving (Show, Eq, Functor)

instance Eq1 RangeExprF where
  liftEq eq (Invert a) (Invert b) = eq a b
  liftEq eq (Union a c) (Union b d) = eq a b && eq c d
  liftEq eq (Intersection a c) (Intersection b d) = eq a b && eq c d
  liftEq eq (Difference a c) (Difference b d) = eq a b && eq c d
  liftEq _ _ _ = False

instance Show1 RangeExprF where
  liftShowsPrec showPrec _ p =
    \case
      Invert x -> showString "not " . showParen True (showPrec (p + 1) x)
      Union a b ->
        showPrec (p + 1) a
          . showString " \\/ "
          . showPrec (p + 1) b
      Intersection a b ->
        showPrec (p + 1) a
          . showString " /\\ "
          . showPrec (p + 1) b
      Difference a b ->
        showPrec (p + 1) a
          . showString " - "
          . showPrec (p + 1) b

newtype RangeExpr a = RangeExpr {getFree :: Free RangeExprF a}
  deriving (Show, Eq, Functor)

-- | This is an F-Algebra. You don't need to know what this is in order to be able
-- to use this module, but, if you are interested you can
-- <https://www.schoolofhaskell.com/user/bartosz/understanding-algebras read more on School of Haskell>.
type Algebra f a = f a -> a

rangeMergeAlgebra :: (Ord a) => Algebra RangeExprF (RangeMerge a)
rangeMergeAlgebra =
  \case
    Invert a -> invertRM a
    Union a b -> a `unionRangeMerges` b
    Intersection a b -> a `intersectionRangeMerges` b
    Difference a b -> a `intersectionRangeMerges` invertRM b
