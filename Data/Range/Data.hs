{-# LANGUAGE Safe #-}

-- | The Data module for common data types within the code.
module Data.Range.Data where

data OverlapType = Separate | Overlap | Adjoin
   deriving (Eq, Show)

data BoundType = Inclusive | Exclusive
   deriving (Eq, Show)

data Bound a = Bound
   { boundValue :: a
   , boundType :: BoundType
   } deriving (Eq, Show)

-- TODO can we implement Monoid for Range a with the addition of an empty?
-- Or maybe we can implement Monoid for a list of ranges...

-- | The Range Data structure; it is capable of representing any type of range. This is
-- the primary data structure in this library. Everything should be possible to convert
-- back into this datatype. All ranges in this structure are inclusively bound.
data Range a
   = SingletonRange a               -- ^ Represents a single element as a range.
   | SpanRange (Bound a) (Bound a)  -- ^ Represents a bounded and inclusive range of elements. The first argument is expected to be less than or equal to the second argument.
   | LowerBoundRange (Bound a)      -- ^ Represents a range with only an inclusive lower bound.
   | UpperBoundRange (Bound a)      -- ^ Represents a range with only an inclusive upper bound.
   | InfiniteRange                  -- ^ Represents an infinite range over all values.
   deriving(Eq)

instance Show a => Show (Range a) where
   showsPrec i (SingletonRange a) = ((++) "SingletonRange ") . showsPrec i a
   showsPrec i (SpanRange (Bound l lType) (Bound r rType)) =
      showsPrec i l . showSymbol lType rType . showsPrec i r
      where
         showSymbol Inclusive Inclusive = (++) " +=+ "
         showSymbol Inclusive Exclusive = (++) " +=* "
         showSymbol Exclusive Inclusive = (++) " *=+ "
         showSymbol Exclusive Exclusive = (++) " *=* "
   showsPrec i (LowerBoundRange (Bound a Inclusive)) = ((++) "lbi ") . (showsPrec i a)
   showsPrec i (LowerBoundRange (Bound a Exclusive)) = ((++) "lbe ") . (showsPrec i a)
   showsPrec i (UpperBoundRange (Bound a Inclusive)) = ((++) "ubi ") . (showsPrec i a)
   showsPrec i (UpperBoundRange (Bound a Exclusive)) = ((++) "ube ") . (showsPrec i a)
   showsPrec i (InfiniteRange) = (++) "inf"
