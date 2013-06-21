module Data.Range.RangeInternal where

import Data.List (sortBy, insertBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)

import Data.Range.Data
import Data.Range.Spans
import Data.Range.Util

{-
 - The following assumptions must be maintained at the beginning of these internal
 - functions so that we can reason about what we are given.
 -
 - RangeMerge assumptions:
 - * The span ranges will never overlap the bounds. 
 - * The span ranges are always sorted in ascending order by the first element.
 - * The lower and upper bounds never overlap in such a way to make it an infinite range.
 -}
data RangeMerge a = RM
   { largestLowerBound :: Maybe a
   , largestUpperBound :: Maybe a
   , spanRanges :: [(a, a)]
   }
   | IRM
   deriving (Show)

-- This function adds an existing range into a range merge
storeRange :: (Ord a) => Range a -> RangeMerge a -> RangeMerge a
storeRange InfiniteRange rm = IRM
storeRange (LowerBoundRange lower) rm = case largestLowerBound rm of
   Just currentLowest -> rm { largestLowerBound = Just $ min lower currentLowest }
   Nothing -> rm { largestLowerBound = Just lower }
storeRange (UpperBoundRange upper) rm = case largestUpperBound rm of
   Just currentUpper -> rm { largestUpperBound = Just $ max upper currentUpper }
   Nothing -> rm { largestUpperBound = Just upper }
storeRange (SpanRange x y) rm = rm { spanRanges = (x, y) `insertSpan` spanRanges rm }
storeRange (SingletonRange x) rm = rm { spanRanges = (x, x) `insertSpan` spanRanges rm }

insertSpan :: Ord a => (a, b) -> [(a, b)] -> [(a, b)]
insertSpan = insertBy (comparing fst)

storeRanges :: (Ord a) => RangeMerge a -> [Range a] -> RangeMerge a
storeRanges = foldr storeRange

loadRanges :: (Ord a) => [Range a] -> RangeMerge a
loadRanges = storeRanges emptyRangeMerge


emptyRangeMerge :: RangeMerge a
emptyRangeMerge = RM Nothing Nothing []

intersectSpansRM :: (Ord a) => RangeMerge a -> RangeMerge a -> RangeMerge a
intersectSpansRM one two = RM Nothing Nothing newSpans
   where
      newSpans = intersectSpans $ insertionSortSpans (spanRanges one) (spanRanges two) 

-- This function assumes that you are given sorted input from which to intersect
intersectSpans :: (Ord a) => [(a, a)] -> [(a, a)]
intersectSpans (f@(a, b) : s@(x, y) : xs) = if isBetween x f
   then (max a x, min b y) : intersectSpans remainder
   else intersectSpans remainder
   where
      remainder = s : xs
intersectSpans _ = [] -- There is nothing left to intersect with, return nothing.

intersectWith :: (Ord a) => (a -> (a, a) -> Maybe (a, a)) -> Maybe a -> [(a, a)] -> [(a, a)]
intersectWith _ Nothing _ = []
intersectWith fix (Just lower) xs = catMaybes $ fmap (fix lower) xs

fixLower :: (Ord a) => a -> (a, a) -> Maybe (a, a)
fixLower lower (x, y) = if lower <= y
   then Just (max lower x, y)
   else Nothing

fixUpper :: (Ord a) => a -> (a, a) -> Maybe (a, a)
fixUpper upper (x, y) = if x <= upper
   then Just (x, min y upper)
   else Nothing

intersectionRangeMerges :: (Ord a, Enum a) => RangeMerge a -> RangeMerge a -> RangeMerge a
intersectionRangeMerges IRM two = two
intersectionRangeMerges one IRM = one
intersectionRangeMerges one two = RM
   { largestLowerBound = newLowerBound
   , largestUpperBound = newUpperBound
   , spanRanges = joinedSpans
   }
   where 
      lowerOneSpans = intersectWith fixLower (largestLowerBound one) (spanRanges two)
      lowerTwoSpans = intersectWith fixLower (largestLowerBound two) (spanRanges one)
      upperOneSpans = intersectWith fixUpper (largestUpperBound one) (spanRanges two)
      upperTwoSpans = intersectWith fixUpper (largestUpperBound two) (spanRanges one)
      intersectedSpans = intersectSpans $ insertionSortSpans (spanRanges one) (spanRanges two) 

      sortedResults = foldr1 insertionSortSpans 
         [ lowerOneSpans
         , lowerTwoSpans
         , upperOneSpans
         , upperTwoSpans
         , intersectedSpans
         ]

      joinedSpans = joinSpans . unionSpans $ sortedResults

      newLowerBound = calculateNewBound largestLowerBound max one two
      newUpperBound = calculateNewBound largestUpperBound min one two

      calculateNewBound 
         :: (Ord a) 
         => (RangeMerge a -> Maybe a) 
         -> (a -> a -> a) 
         -> RangeMerge a -> RangeMerge a -> Maybe a
      calculateNewBound ext comp one two = case (ext one, ext two) of
         (Just x, Just y) -> Just $ comp x y
         (z, Nothing) -> Nothing
         (Nothing, z) -> Nothing

unionRangeMerges :: (Ord a, Enum a) => RangeMerge a -> RangeMerge a -> RangeMerge a
unionRangeMerges IRM _ = IRM
unionRangeMerges _ IRM = IRM
unionRangeMerges one two = foldr appendSpanRM boundedRM joinedSpans
   where
      newLowerBound = calculateNewBound largestLowerBound min one two
      newUpperBound = calculateNewBound largestUpperBound max one two

      sortedSpans = insertionSortSpans (spanRanges one) (spanRanges two)
      joinedSpans = joinSpans . unionSpans $ sortedSpans

      boundedRM = RM
         { largestLowerBound = newLowerBound
         , largestUpperBound = newUpperBound
         , spanRanges = []
         }

      calculateNewBound 
         :: (Ord a) 
         => (RangeMerge a -> Maybe a) 
         -> (a -> a -> a) 
         -> RangeMerge a -> RangeMerge a -> Maybe a
      calculateNewBound ext comp one two = case (ext one, ext two) of
         (Just x, Just y) -> Just $ comp x y
         (z, Nothing) -> z
         (Nothing, z) -> z

appendSpanRM :: (Ord a, Enum a) => (a, a) -> RangeMerge a -> RangeMerge a
appendSpanRM _ IRM = IRM
appendSpanRM sp@(lower, higher) rm = 
   if (newUpper, newLower) == (lub, llb)
      then newRangesRM
         { spanRanges = sp : spanRanges rm
         }
      else newRangesRM
   where
      newRangesRM = rm 
         { largestLowerBound = newLower
         , largestUpperBound = newUpper
         }

      lub = largestUpperBound rm
      llb = largestLowerBound rm

      newLower = do
         bound <- llb
         if bound <= higher
            then return (min bound lower)
            else return bound

      newUpper = do
         bound <- lub
         if lower <= bound
            then return (max bound higher)
            else return bound

{-
unionRange :: (Ord a) => Range a -> RangeMerge a -> RangeMerge a
unionRange InfiniteRange rm = IRM
unionRange (LowerBoundRange lower) rm = case largestLowerBound rm of
   Just currentLowest -> rm { largestLowerBound = Just $ min lower currentLowest }
   Nothing -> rm { largestLowerBound = Just lower }
-}

{-
intersectSpansRM :: (Ord a) => RangeMerge a -> (a, a) -> [(a, a)]
intersectSpansRM rm sp@(lower, upper) = intersectedSpans
   where 
      spans = spanRanges rm
      intersectedSpans = catMaybes $ map (intersectCompareSpan sp) spans

      largestSpan :: Ord a => [(a, a)] -> [(a, a)]
      largestSpan [] = []
      largestSpan xs = (foldr1 (\(l, m) (x, y) -> (min l x, max m y)) xs) : []

intersectCompareSpan :: Ord a => (a, a) -> (a, a) -> Maybe (a, a)
intersectCompareSpan f@(l, m) s@(x, y) = if isBetween l s || isBetween m s
   then Just (max l x, min m y)
   else Nothing
-}

