{-# LANGUAGE Safe #-}

module Data.Range.Util where

import Data.Maybe (catMaybes)

import Data.Range.Data

-- This module is supposed to contain all of the functions that are required by the rest
-- of the code but could be easily pulled into separate and completely non-related
-- codebases or libraries.

compareLower :: Ord a => Bound a -> Bound a -> Ordering
compareLower ab@(Bound a aType) bb@(Bound b bType)
   | ab == bb     = EQ
   | a == b       = if aType == Inclusive then LT else GT
   | a < b        = LT
   | otherwise    = GT

compareHigher :: Ord a => Bound a -> Bound a -> Ordering
compareHigher ab@(Bound a aType) bb@(Bound b bType)
   | ab == bb     = EQ
   | a == b       = if aType == Inclusive then GT else LT
   | a < b        = LT
   | otherwise    = GT

minBounds :: Ord a => Bound a -> Bound a -> Bound a
minBounds ao bo = if compareLower ao bo == LT then ao else bo

maxBounds :: Ord a => Bound a -> Bound a -> Bound a
maxBounds ao bo = if compareHigher ao bo == GT then ao else bo

insertionSort :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
insertionSort comp xs ys = go xs ys
   where
      go (f : fs) (s : ss) = case comp f s of
         LT -> f : go fs (s : ss)
         EQ -> f : s : go fs ss
         GT -> s : go (f : fs) ss
      go [] z = z
      go z [] = z

isBetween :: (Ord a) => a -> (a, a) -> Bool
isBetween a (x, y) = (x <= a) && (a <= y)

invertBound :: Bound a -> Bound a
invertBound (Bound x Inclusive) = Bound x Exclusive
invertBound (Bound x Exclusive) = Bound x Inclusive

boundsOverlapType :: Ord a => (Bound a, Bound a) -> (Bound a, Bound a) -> OverlapType
boundsOverlapType (a, b) (x, y) = (a `boundIsBetween` (x, y)) `orOverlapType` (x `boundIsBetween` (a, b))

orOverlapType :: OverlapType -> OverlapType -> OverlapType
orOverlapType Overlap _ = Overlap
orOverlapType _ Overlap = Overlap
orOverlapType Adjoin _ = Adjoin
orOverlapType _ Adjoin = Adjoin
orOverlapType _ _ = Separate

pointJoinType :: BoundType -> BoundType -> OverlapType
pointJoinType Inclusive Inclusive = Overlap
pointJoinType Exclusive Exclusive = Separate
pointJoinType _ _ = Adjoin

-- This function assumes that the bound on the left is a lower bound and that the range is in (lower, upper)
-- bound order
boundCmp :: (Ord a) => Bound a -> (Bound a, Bound a) -> Ordering
boundCmp ab@(Bound a aType) (xb@(Bound x xType), yb@(Bound y yType))
   | boundIsBetween ab (xb, yb) == Overlap = EQ
   | a < x = LT
   | otherwise = GT

boundIsBetween :: (Ord a) => Bound a -> (Bound a, Bound a) -> OverlapType
boundIsBetween (Bound a aType) (Bound x xType, Bound y yType)
   | x > a     = Separate
   | x == a    = pointJoinType aType yType
   | a < y     = Overlap
   | a == y    = pointJoinType aType yType
   | otherwise = Separate

singletonInSpan :: Ord a => a -> (Bound a, Bound a) -> OverlapType
singletonInSpan a span' = boundIsBetween (Bound a Inclusive) span'

againstLowerBound :: Ord a => Bound a -> Bound a -> OverlapType
againstLowerBound (Bound a aType) (Bound lower lowerType)
   | lower == a   = pointJoinType aType lowerType
   | lower < a    = Overlap
   | otherwise    = Separate

againstUpperBound :: Ord a => Bound a -> Bound a -> OverlapType
againstUpperBound (Bound a aType) (Bound upper upperType)
   | upper == a   = pointJoinType aType upperType
   | a < upper    = Overlap
   | otherwise    = Separate

isBetweenBounds :: (Ord a) => a -> (Bound a, Bound a) -> Bool
isBetweenBounds a (x, y) = (a `isGreaterThan` x) && (a `isLessThan` y)

isLessThan :: (Ord a) => a -> Bound a -> Bool
isLessThan a (Bound x Inclusive) = a <= x
isLessThan a (Bound x Exclusive) = a < x

isGreaterThan :: (Ord a) => a -> Bound a -> Bool
isGreaterThan a (Bound x Inclusive) = a >= x
isGreaterThan a (Bound x Exclusive) = a > x

takeEvenly :: [[a]] -> [a]
takeEvenly [] = []
takeEvenly xss = (catMaybes . map safeHead $ xss) ++ takeEvenly (filter (not . null) . map tail $ xss)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs (tail xs)

lowestValueInLowerBound :: Enum a => Bound a -> a
lowestValueInLowerBound (Bound a Inclusive) = a
lowestValueInLowerBound (Bound a Exclusive) = succ a

highestValueInUpperBound :: Enum a => Bound a -> a
highestValueInUpperBound (Bound a Inclusive) = a
highestValueInUpperBound (Bound a Exclusive) = pred a