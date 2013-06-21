-- This module contains all of the public data structures that need to be avaliable to
-- people wanting to use the code.
module Data.Range.Data where

data Range a
   = SingletonRange a
   | SpanRange a a
   | LowerBoundRange a
   | UpperBoundRange a
   | InfiniteRange
   deriving(Eq, Show)
