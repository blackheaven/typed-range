module Data.Range.Range (
      Range(..),
      inRange,
      rangesOverlap,
      mergeRanges,
      fromRanges,
      fromMergedRanges
   ) where

-- TODO flip range
-- TODO invert range (invent not range)

import Data.Ord (comparing)
import Data.List (sortBy, foldl)
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes)

data Range a
   = SingletonRange a
   | SpanRange a a
   | LowerBoundRange a
   | UpperBoundRange a
   | InfiniteRange
   deriving(Eq, Show)

data RangeMerge a = RM
   { largestLowerBound :: Maybe a
   , largestUpperBound :: Maybe a
   , spanRanges :: [(a, a)]
   }
   | IRM
   deriving (Show)

emptyRangeMerge :: RangeMerge a
emptyRangeMerge = RM Nothing Nothing []

-- an intersection of a value followed by a union of that value should be the identity.
-- This is false. An intersection of a value followed by a union of that value should be
-- the value itself.
-- (1, 3) union (3, 4) => (1, 4)
-- (1, 3) intersection (3, 4) = (3, 3)
-- ((1, 3) intersection (3, 4)) union (3, 4) => (3, 4)

unionRange :: (Ord a) => Range a -> RangeMerge a -> RangeMerge a
unionRange InfiniteRange rm = IRM
unionRange (LowerBoundRange lower) rm = case largestLowerBound rm of
   Just currentLowest -> rm { largestLowerBound = Just $ min lower currentLowest }
   Nothing -> rm { largestLowerBound = Just lower }

-- If it was an infinite range then it should not be after an intersection unless it was
-- an intersection with another infinite range.
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
   , spanRanges = joinMergeSortSpans . ((lower, upper) :) . spanRanges $ rm
   }
intersectionRange (SingletonRange value) rm = intersectionRange (SpanRange value value) rm
   -- You need to update the spans using the new bound that has been added in. Every span
   -- needs to be updated.

joinMergeSortSpans :: (Ord a, Enum a) => [(a, a)] -> [(a, a)]
joinMergeSortSpans = joinSpans . mergeSpans . sortSpans

sortSpans :: (Ord a) => [(a, a)] -> [(a, a)]
sortSpans = sortBy (comparing fst)

mergeSpans :: Ord a => [(a, a)] -> [(a, a)]
mergeSpans (f@(a, b) : s@(x, y) : xs) = if isBetween x f 
   then mergeSpans ((a, max b y) : xs)
   else f : mergeSpans (s : xs)
mergeSpans xs = xs
         
joinSpans :: (Ord a, Enum a) => [(a, a)] -> [(a, a)]
joinSpans (f@(a, b) : s@(x, y) : xs) = 
   if succ b == x
      then joinSpans $ (a, y) : xs
      else f : joinSpans (s : xs)
joinSpans xs = xs

-- OLD Code Below

storeRange :: (Ord a) => Range a -> RangeMerge a -> RangeMerge a
storeRange InfiniteRange rm = IRM
storeRange (LowerBoundRange lower) rm = case largestLowerBound rm of
   Just currentLowest -> rm { largestLowerBound = Just $ min lower currentLowest }
   Nothing -> rm { largestLowerBound = Just lower }
storeRange (UpperBoundRange upper) rm = case largestUpperBound rm of
   Just currentUpper -> rm { largestUpperBound = Just $ max upper currentUpper }
   Nothing -> rm { largestUpperBound = Just upper }
storeRange (SpanRange x y) rm = rm { spanRanges = (x, y) : spanRanges rm }
storeRange (SingletonRange x) rm = rm { spanRanges = (x, x) : spanRanges rm }

storeRanges :: (Ord a) => RangeMerge a -> [Range a] -> RangeMerge a
storeRanges = foldr storeRange

loadRanges :: (Ord a) => [Range a] -> RangeMerge a
loadRanges = storeRanges emptyRangeMerge

-- Assume that the compression returns a sorted list

-- Should use this when a span or bound update happens...so after any update happens at
-- all
potentiallyMergeBounds :: (Enum a, Ord a) => RangeMerge a -> RangeMerge a
potentiallyMergeBounds rm = case (upper, lower) of
   (Just high, Just low) -> if succ high >= low then IRM else rm
   _ -> rm
   where
      upper = largestUpperBound rm
      lower = largestLowerBound rm

optimizeRangeMerge :: (Ord a, Enum a) => RangeMerge a -> RangeMerge a
optimizeRangeMerge = potentiallyMergeBounds

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

rangesOverlap :: (Ord a) => Range a -> Range a -> Bool
rangesOverlap (SingletonRange a) (SingletonRange b) = a == b
rangesOverlap (SingletonRange a) (SpanRange x y) = isBetween a (x, y)
rangesOverlap (SingletonRange a) (LowerBoundRange lower) = lower <= a
rangesOverlap (SingletonRange a) (UpperBoundRange upper) = a <= upper
rangesOverlap (SpanRange x y) (SpanRange a b) = isBetween x (a, b) || isBetween a (x, y)
rangesOverlap (SpanRange _ y) (LowerBoundRange lower) = lower <= y
rangesOverlap (SpanRange x _) (UpperBoundRange upper) = x <= upper
rangesOverlap (LowerBoundRange _) (LowerBoundRange _) = True
rangesOverlap (LowerBoundRange x) (UpperBoundRange y) = x <= y
rangesOverlap (UpperBoundRange _) (UpperBoundRange _) = True
rangesOverlap InfiniteRange _ = True
rangesOverlap a b = rangesOverlap b a

-- x- y- will both overlap
-- 3- -9 will overlap
-- x- -y will overlap if x <= y

inRange :: (Ord a) => Range a -> a -> Bool
inRange (SingletonRange a) value = value == a
inRange (SpanRange x y) value = isBetween value (x, y)
inRange (LowerBoundRange lower) value = lower <= value
inRange (UpperBoundRange upper) value = value <= upper
inRange InfiniteRange _ = True

isBetween :: (Ord a) => a -> (a, a) -> Bool
isBetween a (x, y) = (x <= a) && (a <= y)

mergeRanges :: (Ord a, Enum a) => [Range a] -> [Range a]
mergeRanges = exportRangeMerge . loadRanges

takeEvenly :: [a] -> [a] -> [a]
takeEvenly x [] = x
takeEvenly [] x = x
takeEvenly (a:as) (b:bs) = a : b : takeEvenly as bs

fromRanges :: (Ord a, Enum a) => [Range a] -> [a]
fromRanges = concatMap fromRange
   where 
      fromRange range = case range of 
         SingletonRange x -> [x] 
         SpanRange a b -> [a..b]
         LowerBoundRange x -> iterate succ x
         UpperBoundRange x -> iterate pred x
         InfiniteRange -> zero : takeEvenly (tail $ iterate succ zero) (tail $ iterate pred zero)
            where
               zero = toEnum 0

fromMergedRanges :: (Ord a, Enum a) => [Range a] -> [a]
fromMergedRanges = fromRanges . mergeRanges
