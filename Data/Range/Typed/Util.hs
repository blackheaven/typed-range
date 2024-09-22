{-# LANGUAGE LambdaCase #-}

module Data.Range.Typed.Util where

import Data.Maybe (mapMaybe)
import Data.Range.Typed.Data
import Optics.Lens (Lens', lens)

-- This module is supposed to contain all of the functions that are required by the rest
-- of the code but could be easily pulled into separate and completely non-related
-- codebases or libraries.

compareLower :: (Ord a) => Bound a -> Bound a -> Ordering
compareLower a b
  | a == b = EQ
  | boundValue a == boundValue b = if boundIsInclusive a then LT else GT
  | boundValue a < boundValue b = LT
  | otherwise = GT

compareHigher :: (Ord a) => Bound a -> Bound a -> Ordering
compareHigher a b
  | a == b = EQ
  | boundValue a == boundValue b = if boundIsInclusive a then GT else LT
  | boundValue a < boundValue b = LT
  | otherwise = GT

compareLowerIntersection :: (Ord a) => Bound a -> Bound a -> Ordering
compareLowerIntersection a b
  | a == b = EQ
  | boundValue a == boundValue b = if boundIsInclusive a then GT else LT
  | boundValue a < boundValue b = LT
  | otherwise = GT

compareHigherIntersection :: (Ord a) => Bound a -> Bound a -> Ordering
compareHigherIntersection a b
  | a == b = EQ
  | boundValue a == boundValue b = if boundIsInclusive a then LT else GT
  | boundValue a < boundValue b = LT
  | otherwise = GT

compareUpperToLower :: (Ord a) => Bound a -> Bound a -> Ordering
compareUpperToLower upper lower
  | boundValue upper == boundValue lower = if boundIsInclusive upper || boundIsInclusive lower then EQ else LT
  | boundValue upper < boundValue lower = LT
  | otherwise = GT

minBounds :: (Ord a) => Bound a -> Bound a -> Bound a
minBounds ao bo = if compareLower ao bo == LT then ao else bo

maxBounds :: (Ord a) => Bound a -> Bound a -> Bound a
maxBounds ao bo = if compareHigher ao bo == GT then ao else bo

minBoundsIntersection :: (Ord a) => Bound a -> Bound a -> Bound a
minBoundsIntersection ao bo = if compareLowerIntersection ao bo == LT then ao else bo

maxBoundsIntersection :: (Ord a) => Bound a -> Bound a -> Bound a
maxBoundsIntersection ao bo = if compareHigherIntersection ao bo == GT then ao else bo

insertionSort :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
insertionSort comp = go
  where
    go (f : fs) (s : ss) = case comp f s of
      LT -> f : go fs (s : ss)
      EQ -> f : s : go fs ss
      GT -> s : go (f : fs) ss
    go [] z = z
    go z [] = z

invertBound :: Bound a -> Bound a
invertBound (InclusiveBound x) = ExclusiveBound x
invertBound (ExclusiveBound x) = InclusiveBound x

isEmptySpan :: (Eq a) => (Bound a, Bound a) -> Bool
isEmptySpan (a, b) = boundValue a == boundValue b && (not (boundIsInclusive a) || not (boundIsInclusive b))

removeEmptySpans :: (Eq a) => [(Bound a, Bound a)] -> [(Bound a, Bound a)]
removeEmptySpans = filter (not . isEmptySpan)

boundsOverlapType :: (Ord a) => (Bound a, Bound a) -> (Bound a, Bound a) -> OverlapType
boundsOverlapType l@(a, b) r@(x, y)
  | isEmptySpan l || isEmptySpan r = Separate
  | boundValue a == boundValue x = Overlap
  | boundValue b == boundValue y = Overlap
  | otherwise = (a `boundIsBetween` (x, y)) `orOverlapType` (x `boundIsBetween` (a, b))

orOverlapType :: OverlapType -> OverlapType -> OverlapType
orOverlapType Overlap _ = Overlap
orOverlapType _ Overlap = Overlap
orOverlapType Adjoin _ = Adjoin
orOverlapType _ Adjoin = Adjoin
orOverlapType _ _ = Separate

pointJoinType :: Bound a -> Bound b -> OverlapType
pointJoinType (InclusiveBound _) (InclusiveBound _) = Overlap
pointJoinType (ExclusiveBound _) (ExclusiveBound _) = Separate
pointJoinType _ _ = Adjoin

-- This function assumes that the bound on the left is a lower bound and that the range is in (lower, upper)
-- bound order
boundCmp :: (Ord a) => Bound a -> (Bound a, Bound a) -> Ordering
boundCmp a (x, y)
  | boundIsBetween a (x, y) /= Separate = EQ
  | boundValue a <= boundValue x = LT
  | otherwise = GT

-- TODO replace everywhere with boundsOverlapType
boundIsBetween :: (Ord a) => Bound a -> (Bound a, Bound a) -> OverlapType
boundIsBetween a (x, y)
  | boundValue x > boundValue a = Separate
  | boundValue x == boundValue a = pointJoinType a x
  | boundValue a < boundValue y = Overlap
  | boundValue a == boundValue y = pointJoinType a y
  | otherwise = Separate

singletonInSpan :: (Ord a) => a -> (Bound a, Bound a) -> OverlapType
singletonInSpan a = boundIsBetween $ InclusiveBound a

againstLowerBound :: (Ord a) => Bound a -> Bound a -> OverlapType
againstLowerBound a lower
  | boundValue lower == boundValue a = pointJoinType a lower
  | boundValue lower < boundValue a = Overlap
  | otherwise = Separate

againstUpperBound :: (Ord a) => Bound a -> Bound a -> OverlapType
againstUpperBound a upper
  | boundValue upper == boundValue a = pointJoinType a upper
  | boundValue a < boundValue upper = Overlap
  | otherwise = Separate

takeEvenly :: [[a]] -> [a]
takeEvenly [] = []
takeEvenly xss = mapMaybe safeHead xss <> takeEvenly (filter (not . null) $ map tail xss)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs (tail xs)

lowestValueInLowerBound :: (Enum a) => Bound a -> a
lowestValueInLowerBound = boundValueNormalized succ

highestValueInUpperBound :: (Enum a) => Bound a -> a
highestValueInUpperBound = boundValueNormalized pred

boundValue :: Bound a -> a
boundValue =
  \case
    InclusiveBound a -> a
    ExclusiveBound a -> a

boundValueNormalized :: (a -> a) -> Bound a -> a
boundValueNormalized normalize =
  \case
    InclusiveBound a -> a
    ExclusiveBound a -> normalize a

boundIsInclusive :: Bound a -> Bool
boundIsInclusive =
  \case
    InclusiveBound _ -> True
    ExclusiveBound _ -> False

-- | Changing `Range`'s lower bound (possibly changing the constructor)
lowerBoundUnstable :: Lens' (AnyRange a) (Maybe (Bound a))
lowerBoundUnstable = lens (\(AnyRangeFor range) -> g range) (\(AnyRangeFor range) -> s range)
  where
    g :: Range hasLowerBound hasUpperBound a -> Maybe (Bound a)
    g =
      \case
        SingletonRange a -> Just $ InclusiveBound a
        SpanRange x _ -> Just x
        LowerBoundRange x -> Just x
        UpperBoundRange _ -> Nothing
        InfiniteRange -> Nothing
        EmptyRange -> Nothing
    s :: Range hasLowerBound hasUpperBound a -> Maybe (Bound a) -> AnyRange a
    s =
      \case
        SingletonRange _ ->
          \case
            Just (InclusiveBound y) -> AnyRangeFor $ SingletonRange y
            Just (ExclusiveBound y) -> AnyRangeFor $ SingletonRange y
            Nothing -> AnyRangeFor EmptyRange
        SpanRange _ x -> maybe (AnyRangeFor $ UpperBoundRange x) (AnyRangeFor . (`SpanRange` x))
        LowerBoundRange _ -> maybe (AnyRangeFor InfiniteRange) (AnyRangeFor . LowerBoundRange)
        UpperBoundRange x -> maybe (AnyRangeFor $ UpperBoundRange x) (AnyRangeFor . (`SpanRange` x))
        InfiniteRange -> maybe (AnyRangeFor InfiniteRange) (AnyRangeFor . LowerBoundRange)
        EmptyRange -> const $ AnyRangeFor EmptyRange

-- | Changing `Range`'s upper bound (possibly changing the constructor)
upperBoundUnstable :: Lens' (AnyRange a) (Maybe (Bound a))
upperBoundUnstable = lens (\(AnyRangeFor range) -> g range) (\(AnyRangeFor range) -> s range)
  where
    g :: Range hasLowerBound hasUpperBound a -> Maybe (Bound a)
    g =
      \case
        SingletonRange a -> Just $ InclusiveBound a
        SpanRange _ x -> Just x
        UpperBoundRange x -> Just x
        LowerBoundRange _ -> Nothing
        InfiniteRange -> Nothing
        EmptyRange -> Nothing
    s :: Range hasLowerBound hasUpperBound a -> Maybe (Bound a) -> AnyRange a
    s =
      \case
        SingletonRange _ ->
          \case
            Just (InclusiveBound y) -> AnyRangeFor $ SingletonRange y
            Just (ExclusiveBound y) -> AnyRangeFor $ SingletonRange y
            Nothing -> AnyRangeFor EmptyRange
        SpanRange x _ -> maybe (AnyRangeFor $ UpperBoundRange x) (AnyRangeFor . SpanRange x)
        UpperBoundRange _ -> maybe (AnyRangeFor InfiniteRange) (AnyRangeFor . UpperBoundRange)
        LowerBoundRange x -> maybe (AnyRangeFor $ LowerBoundRange x) (AnyRangeFor . SpanRange x)
        InfiniteRange -> maybe (AnyRangeFor InfiniteRange) (AnyRangeFor . UpperBoundRange)
        EmptyRange -> const $ AnyRangeFor EmptyRange
