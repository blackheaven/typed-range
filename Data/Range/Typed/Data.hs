{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The Data module for common data types within the code.
module Data.Range.Typed.Data where

import Data.Kind
import Optics.Getter (view)
import Optics.Lens (Lens', lens)
import Optics.Setter (set)

data OverlapType = Separate | Overlap | Adjoin
  deriving (Eq, Show)

-- | Represents a bound, with exclusiveness.
data Bound a
  = -- | The value should be included in the bound.
    InclusiveBound a
  | -- | The value should be excluded in the bound.
    ExclusiveBound a
  deriving stock (Eq, Show, Functor)

-- | All kinds of ranges.
data Range (hasLowerBound :: Bool) (hasUpperBound :: Bool) (a :: Type) where
  -- | A single element. It is equivalent to @SpanRange (InclusiveBound a) (InclusiveBound a)@.
  SingletonRange :: a -> Range 'True 'True a
  -- | A span of elements. Make sure lower bound <= upper bound.
  SpanRange :: Bound a -> Bound a -> Range 'True 'True a
  -- | A range with a finite lower bound and an infinite upper bound.
  LowerBoundRange :: Bound a -> Range 'True 'False a
  -- | A range with an infinite lower bound and a finite upper bound.
  UpperBoundRange :: Bound a -> Range 'False 'True a
  -- | An infinite range.
  InfiniteRange :: Range 'False 'False a
  -- | An empty range.
  EmptyRange :: Range 'False 'False a

deriving stock instance (Eq a) => Eq (Range l r a)

deriving stock instance Functor (Range l r)

instance (Show a) => Show (Range r l a) where
  showsPrec i =
    \case
      SingletonRange a -> (<>) "singleton " . showsPrec i a
      SpanRange lBound rBound ->
        let s l symbol r = showsPrec i l . (<>) symbol . showsPrec i r
         in case (lBound, rBound) of
              (InclusiveBound l, InclusiveBound r) -> s l " +=+ " r
              (InclusiveBound l, ExclusiveBound r) -> s l " +=* " r
              (ExclusiveBound l, InclusiveBound r) -> s l " *=+ " r
              (ExclusiveBound l, ExclusiveBound r) -> s l " *=* " r
      (LowerBoundRange (InclusiveBound a)) -> (<>) "lbi " . showsPrec i a
      (LowerBoundRange (ExclusiveBound a)) -> (<>) "lbe " . showsPrec i a
      (UpperBoundRange (InclusiveBound a)) -> (<>) "ubi " . showsPrec i a
      (UpperBoundRange (ExclusiveBound a)) -> (<>) "ube " . showsPrec i a
      InfiniteRange -> (<>) "inf"
      EmptyRange -> (<>) "empty"

type AnyRange = AnyRangeFor AnyRangeConstraint

class AnyRangeConstraint (range :: Type -> Type)

instance AnyRangeConstraint (Range l r)

data AnyRangeFor (c :: (Type -> Type) -> Constraint) a
  = forall hasLowerBound hasUpperBound.
    (c (Range hasLowerBound hasUpperBound)) =>
    AnyRangeFor (Range hasLowerBound hasUpperBound a)

instance (Show a) => Show (AnyRangeFor c a) where
  showsPrec i (AnyRangeFor range) =
    showsPrec i range

instance (Eq a) => Eq (AnyRangeFor c a) where
  AnyRangeFor lower == AnyRangeFor upper =
    case (lower, upper) of
      (SingletonRange l, SingletonRange r) -> r == l
      (SpanRange ll lr, SpanRange rl rr) -> rl == ll && lr == rr
      (LowerBoundRange l, LowerBoundRange r) -> r == l
      (UpperBoundRange l, UpperBoundRange r) -> r == l
      (InfiniteRange, InfiniteRange) -> True
      (EmptyRange, EmptyRange) -> True
      _ -> False

instance Functor (AnyRangeFor c) where
  fmap i (AnyRangeFor range) =
    AnyRangeFor $ fmap i range

-- | `Range` has a lower bound
class WithLowerBound range where
  -- | Changing `Range`'s lower bound (preserving the constructor)
  lowerBound :: Lens' (range a) (Bound a)

instance WithLowerBound (Range 'True hasUpperBound) where
  lowerBound = lens g s
    where
      g :: Range 'True hasUpperBound a -> Bound a
      g =
        \case
          SingletonRange a -> InclusiveBound a
          SpanRange x _ -> x
          LowerBoundRange x -> x
      s :: Range 'True hasUpperBound a -> Bound a -> Range 'True hasUpperBound a
      s range y =
        case range of
          SingletonRange _ ->
            SingletonRange $
              case y of
                InclusiveBound y' -> y'
                ExclusiveBound y' -> y'
          SpanRange _ x -> SpanRange y x
          LowerBoundRange _ -> LowerBoundRange y

instance WithLowerBound (AnyRangeFor WithLowerBound) where
  lowerBound =
    lens
      (\(AnyRangeFor range) -> view lowerBound range)
      (\(AnyRangeFor range) bound -> AnyRangeFor $ set lowerBound bound range)

instance WithLowerBound (AnyRangeFor WithAllBounds) where
  lowerBound =
    lens
      (\(AnyRangeFor range) -> view lowerBound range)
      (\(AnyRangeFor range) bound -> AnyRangeFor $ set lowerBound bound range)

-- | `Range` has a upper bound
class WithUpperBound range where
  -- | Changing `Range`'s upper bound (preserving the constructor)
  upperBound :: Lens' (range a) (Bound a)

instance WithUpperBound (Range hasLowerBound 'True) where
  upperBound = lens g s
    where
      g :: Range hasLowerBound 'True a -> Bound a
      g =
        \case
          SingletonRange a -> InclusiveBound a
          SpanRange x _ -> x
          UpperBoundRange x -> x
      s :: Range hasLowerBound 'True a -> Bound a -> Range hasLowerBound 'True a
      s range y =
        case range of
          SingletonRange _ ->
            SingletonRange $
              case y of
                InclusiveBound y' -> y'
                ExclusiveBound y' -> y'
          SpanRange x _ -> SpanRange x y
          UpperBoundRange _ -> UpperBoundRange y

instance WithUpperBound (AnyRangeFor WithUpperBound) where
  upperBound =
    lens
      (\(AnyRangeFor range) -> view upperBound range)
      (\(AnyRangeFor range) bound -> AnyRangeFor $ set upperBound bound range)

instance WithUpperBound (AnyRangeFor WithAllBounds) where
  upperBound =
    lens
      (\(AnyRangeFor range) -> view upperBound range)
      (\(AnyRangeFor range) bound -> AnyRangeFor $ set upperBound bound range)

class (WithLowerBound a, WithUpperBound a) => WithAllBounds (a :: Type -> Type)

instance (WithLowerBound a, WithUpperBound a) => WithAllBounds a
