module Data.Range.RangeInternal where

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
   deriving (Show, Eq)

-- This function adds an existing range into a range merge
-- TODO write a test case that asserts that this function always ensures that 
storeRange :: (Ord a) => Range a -> RangeMerge a -> RangeMerge a
storeRange InfiniteRange rm = IRM
-- TODO we should look to see if lenses could clean up this code.
storeRange (LowerBoundRange lower) rm = rm { largestLowerBound = newBound }
   where 
      newBound = Just $ maybe lower (min lower) (largestLowerBound rm)
storeRange (UpperBoundRange upper) rm = rm { largestUpperBound = newBound }
   where
      newBound = Just $ maybe upper (max upper) (largestUpperBound rm)
storeRange (SpanRange x y) rm = rm { spanRanges = (x, y) `insertSpan` spanRanges rm }
storeRange (SingletonRange x) rm = rm { spanRanges = (x, x) `insertSpan` spanRanges rm }

-- TODO Refactor this
potentiallyMergeBounds :: (Enum a, Ord a) => RangeMerge a -> RangeMerge a
potentiallyMergeBounds rm = case (upper, lower) of
   (Just high, Just low) -> if succ high >= low then IRM else rm
   _ -> rm
   where
      upper = largestUpperBound rm
      lower = largestLowerBound rm

optimizeRangeMerge :: (Ord a, Enum a) => RangeMerge a -> RangeMerge a
optimizeRangeMerge = potentiallyMergeBounds
-- END REFACTOR REQUIRED

exportRangeMerge :: (Ord a, Enum a) => RangeMerge a -> [Range a]
exportRangeMerge IRM = [InfiniteRange]
exportRangeMerge rm = putAll $ optimizeRangeMerge rm
   where
      putAll IRM = [InfiniteRange]
      putAll (RM lb up spans) = 
         putLowerBound lb ++ putUpperBound up ++ putSpans spans

      putLowerBound = maybe [] (return . LowerBoundRange)
      putUpperBound = maybe [] (return . UpperBoundRange)
      putSpans = map simplifySpan

      simplifySpan (x, y) = if x == y
         then SingletonRange x
         else SpanRange x y

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

-- TODO when doing a union it is possible that you could end up with an infinite range
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
         return $ if bound <= higher
            then min bound lower
            else bound

      newUpper = do
         bound <- lub
         return $ if lower <= bound
            then max bound higher
            else bound

invertRM :: (Ord a, Enum a) => RangeMerge a -> RangeMerge a
invertRM IRM = emptyRangeMerge
invertRM (RM Nothing Nothing []) = IRM
invertRM (RM (Just lower) Nothing []) = RM Nothing (Just . pred $ lower) []
invertRM (RM Nothing (Just upper) []) = RM (Just . succ $ upper) Nothing []
invertRM (RM (Just lower) (Just upper) []) = RM Nothing Nothing [(succ upper, pred lower)]
invertRM rm = RM
   { largestUpperBound = newUpperBound
   , largestLowerBound = newLowerBound
   , spanRanges = upperSpan ++ betweenSpans ++ lowerSpan
   }
   where
      newLowerValue = succ . snd . last . spanRanges $ rm
      newUpperValue = pred . fst . head . spanRanges $ rm

      newUpperBound = case largestUpperBound rm of
         Just _ -> Nothing
         Nothing -> Just newUpperValue

      newLowerBound = case largestLowerBound rm of
         Just _ -> Nothing
         Nothing -> Just newLowerValue

      upperSpan = case largestUpperBound rm of
         Nothing -> []
         Just upper -> [(succ upper, newUpperValue)]
      lowerSpan = case largestLowerBound rm of
         Nothing -> []
         Just lower -> [(newLowerValue, pred lower)] 

      betweenSpans = invertSpans . spanRanges $ rm

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

-- If it was an infinite range then it should not be after an intersection unless it was
-- an intersection with another infinite range.
{-
intersectionRange :: (Ord a, Enum a) => Range a -> RangeMerge a -> RangeMerge a
intersectionRange InfiniteRange rm = rm -- Intersection with universe remains same
intersectionRange (LowerBoundRange lower) rm = rm
   { largestLowerBound = largestLowerBound rm >>= return . max lower
   , spanRanges = catMaybes . map (updateRange lower) . spanRanges $ rm
   }
   where
      updateRange :: (Ord a) => a -> (a, a) -> Maybe (a, a)
      updateRange lower (begin, end) = if lower <= end
         then Just (max lower begin, end)
         else Nothing
intersectionRange (UpperBoundRange upper) rm = rm
   { largestUpperBound = largestUpperBound rm >>= return . min upper
   , spanRanges = catMaybes . map (updateRange upper) . spanRanges $ rm
   }
   where
      updateRange :: (Ord a) => a -> (a, a) -> Maybe (a, a)
      updateRange upper (begin, end) = if begin <= upper
         then Just (begin, min upper end)
         else Nothing
intersectionRange (SpanRange lower upper) rm = rm
   -- update the bounds first and then update the spans, if the spans were sorted then
   { largestUpperBound = largestUpperBound rm >>= return . min upper
   , largestLowerBound = largestLowerBound rm >>= return . max lower
   -- they would be faster to update I suspect, lets start with not sorted
   , spanRanges = joinUnionSortSpans . ((lower, upper) :) . spanRanges $ rm
   }
   where
      joinUnionSortSpans :: (Ord a, Enum a) => [(a, a)] -> [(a, a)]
      joinUnionSortSpans = joinSpans . unionSpans . sortSpans

intersectionRange (SingletonRange value) rm = intersectionRange (SpanRange value value) rm
-}
