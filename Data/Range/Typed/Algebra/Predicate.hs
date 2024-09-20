{-# LANGUAGE LambdaCase #-}

module Data.Range.Typed.Algebra.Predicate where

import Control.Applicative
import Data.Range.Typed.Algebra.Internal

predicateAlgebra :: Algebra RangeExprF (a -> Bool)
predicateAlgebra =
  \case
    Invert f -> liftA not f
    Union f g -> liftA2 (||) f g
    Intersection f g -> liftA2 (&&) f g
    Difference f g -> liftA2 (&&~) f g
  where
    (&&~) a b = a && not b
