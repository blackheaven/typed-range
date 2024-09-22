{-# LANGUAGE RankNTypes #-}

module Data.Range.Typed.Operators where

import Data.Range.Typed.Data

-- | Mathematically equivalent to @[x, y]@.
--
-- @x +=+ y@ is the short version of @SpanRange (InclusiveBound x) (InclusiveBound y)@
(+=+) :: a -> a -> Range 'True 'True a
(+=+) x y = SpanRange (InclusiveBound x) (InclusiveBound y)

-- | Mathematically equivalent to @[x, y)@.
--
-- @x +=* y@ is the short version of @SpanRange (InclusiveBound x) (ExclusiveBound y)@
(+=*) :: a -> a -> Range 'True 'True a
(+=*) x y = SpanRange (InclusiveBound x) (ExclusiveBound y)

-- | Mathematically equivalent to @(x, y]@.
--
-- @x *=+ y@ is the short version of @SpanRange (ExclusiveBound x) (InclusiveBound y)@
(*=+) :: a -> a -> Range 'True 'True a
(*=+) x y = SpanRange (ExclusiveBound x) (InclusiveBound y)

-- | Mathematically equivalent to @(x, y)@.
--
-- @x *=* y@ is the short version of @SpanRange (ExclusiveBound x) (ExclusiveBound y)@
(*=*) :: a -> a -> Range 'True 'True a
(*=*) x y = SpanRange (ExclusiveBound x) (ExclusiveBound y)

-- | Mathematically equivalent to @[x, Infinity)@.
--
-- @lbi x@ is the short version of @LowerBoundRange (InclusiveBound x)@
lbi :: a -> Range 'True 'False a
lbi = LowerBoundRange . InclusiveBound

-- | Mathematically equivalent to @(x, Infinity)@.
--
-- @lbe x@ is the short version of @LowerBoundRange (ExclusiveBound x)@
lbe :: a -> Range 'True 'False a
lbe = LowerBoundRange . ExclusiveBound

-- | Mathematically equivalent to @(Infinity, x]@.
--
-- @ubi x@ is the short version of @UpperBoundRange (InclusiveBound x)@
ubi :: a -> Range 'False 'True a
ubi = UpperBoundRange . InclusiveBound

-- | Mathematically equivalent to @(Infinity, x)@.
--
-- @ube x@ is the short version of @UpperBoundRange (ExclusiveBound x)@
ube :: a -> Range 'False 'True a
ube = UpperBoundRange . ExclusiveBound

-- | Shorthand for the `InfiniteRange`
inf :: Range 'False 'False a
inf = InfiniteRange

-- | Shorthand for the `EmptyRange`
empty :: Range 'False 'False a
empty = EmptyRange

-- | Shorthand for the `SingletonRange`
singleton :: a -> Range 'True 'True a
singleton = SingletonRange

-- | Shorthand for the `AnyRangeFor`
anyRange :: forall a l h. Range l h a -> AnyRange a
anyRange = AnyRangeFor

-- | Shorthand for the `AnyRangeFor`
anyRangeFor :: forall c a l h. (c (Range l h)) => Range l h a -> AnyRangeFor c a
anyRangeFor = AnyRangeFor

-- | Apply a function over `AnyRangeFor`
withRange :: (forall l h. (c (Range l h)) => Range l h a -> b) -> AnyRangeFor c a -> b
withRange f (AnyRangeFor range) = f range
