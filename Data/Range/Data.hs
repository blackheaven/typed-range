-- | The Data module for common data types within the code.
module Data.Range.Data where

-- | The Range Data structure; it is capable of representing any type of range. This is
-- the primary data structure in this library. Everything should be possible to convert
-- back into this datatype. All ranges in this structure are inclusively bound.
data Range a
   = SingletonRange a      -- ^ Represents a single element as a range.
   | SpanRange a a         -- ^ Represents a bounded and inclusive range of elements.
   | LowerBoundRange a     -- ^ Represents a range with only an inclusive lower bound.
   | UpperBoundRange a     -- ^ Represents a range with only an inclusive upper bound.
   | InfiniteRange         -- ^ Represents an infinite range over all values.
   deriving(Eq, Show)
