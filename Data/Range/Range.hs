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

data Range a
   = SingletonRange a
   | SpanRange a a
   | LowerBoundRange a
   | UpperBoundRange a
   | InfiniteRange
   deriving(Eq, Show)

data RangeMerge a = RM
   { isInfRange :: Bool
   , largestLowerBound :: Maybe a
   , largestUpperBound :: Maybe a
   , spanRanges :: [(a, a)]
   }
   deriving (Show)

emptyRangeMerge :: RangeMerge a
emptyRangeMerge = RM False Nothing Nothing []

storeRange :: (Ord a) => Range a -> RangeMerge a -> RangeMerge a
storeRange InfiniteRange rm = rm { isInfRange = True }
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
joinSpans :: (Ord a, Enum a) => [(a, a)] -> [(a, a)]
joinSpans (f@(a, b) : s@(x, y) : xs) = 
   if succ b == x
      then joinSpans $ (a, y) : xs
      else f : joinSpans (s : xs)
joinSpans xs = xs

joinRangeSpans :: (Ord a, Enum a) => RangeMerge a -> RangeMerge a
joinRangeSpans rm = rm { spanRanges = joinSpans . spanRanges $ rm }

compressSpans :: (Ord a) => RangeMerge a -> RangeMerge a
compressSpans rm = rm { spanRanges = update rm } 
   where
      update = mergeSpans . sortBy (comparing fst) . spanRanges

      mergeSpans :: Ord a => [(a, a)] -> [(a, a)]
      mergeSpans (f@(a, b) : s@(x, y) : xs) = if isBetween x f 
         then mergeSpans ((a, max b y) : xs)
         else f : mergeSpans (s : xs)
      mergeSpans xs = xs
         
updateBounds :: (Ord a, Enum a) => RangeMerge a -> RangeMerge a
updateBounds rm = foldr singleSpanUpdate rmNoSpans (spanRanges rm)
   where
      rmNoSpans = rm { spanRanges = [] }

      singleSpanUpdate :: (Ord a, Enum a) => (a, a) -> RangeMerge a -> RangeMerge a
      singleSpanUpdate o@(x, y) rm = case (updatedLower, updatedUpper) of
         (Nothing, Nothing) -> rm { spanRanges = o : spanRanges rm }
         (a@(Just _), Nothing) -> rm { largestLowerBound = a }
         (Nothing, b@(Just _)) -> rm { largestUpperBound = b }
         (a, b) -> rm
            { largestLowerBound = a
            , largestUpperBound = b
            }
         where
            updatedLower = update (<= succ y) (min x) . largestLowerBound $ rm
            updatedUpper = update (pred x <=) (max y) . largestUpperBound $ rm

      update :: (Ord a) => (a -> Bool) -> (a -> a) -> Maybe a -> Maybe a
      update _ _ Nothing = Nothing
      update compare step (Just value) = if compare value
         then Just $ step value
         else Nothing

optimizeRangeMerge :: (Ord a, Enum a) => RangeMerge a -> RangeMerge a
optimizeRangeMerge = updateBounds . joinRangeSpans . compressSpans

exportRangeMerge :: (Ord a, Enum a) => RangeMerge a -> [Range a]
exportRangeMerge rm = if isInfRange rm
   then [InfiniteRange]
   else case optimizeRangeMerge rm of
      (RM True _ _ _) -> [InfiniteRange]
      orm -> putAll orm
   where
      putAll (RM _ lb up spans) = 
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
