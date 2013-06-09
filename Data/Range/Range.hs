module Data.Range.Range (
      Range(..),
      inRange,
      rangesOverlap,
      mergeRange,
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
   , singletons :: [a]
   }
   deriving (Show)

emptyRangeMerge :: RangeMerge a
emptyRangeMerge = RM False Nothing Nothing [] []

storeRange :: (Ord a) => Range a -> RangeMerge a -> RangeMerge a
storeRange InfiniteRange rm = rm { isInfRange = True }
storeRange (LowerBoundRange lower) rm = case largestLowerBound rm of
   Just currentLowest -> rm { largestLowerBound = Just $ min lower currentLowest }
   Nothing -> rm { largestLowerBound = Just lower }
storeRange (UpperBoundRange upper) rm = case largestUpperBound rm of
   Just currentUpper -> rm { largestUpperBound = Just $ max upper currentUpper }
   Nothing -> rm { largestUpperBound = Just upper }
storeRange (SpanRange x y) rm = rm { spanRanges = (x, y) : spanRanges rm }
storeRange (SingletonRange x) rm = rm { singletons = x : singletons rm }

storeRanges :: (Ord a) => RangeMerge a -> [Range a] -> RangeMerge a
storeRanges = foldr storeRange

loadRanges :: (Ord a) => [Range a] -> RangeMerge a
loadRanges = storeRanges emptyRangeMerge

-- Assume that the list that comes in is sorted
compressSingletons :: (Ord a, Enum a) => [a] -> [Either a (a, a)]
compressSingletons = go [] 
   where
      go :: (Ord a, Enum a) => [Either a (a, a)] -> [a] -> [Either a (a, a)]
      go rs [] = rs
      go [] (x:xs) = go [Left x] xs
      go orig@(Right (low, high):rs) (x:xs) = if succ high >= x
         then go (Right (low, x):rs) xs
         else go (Left x : orig) xs
      go orig@(Left a:rs) (x:xs) = case compare (succ a) x of
         LT -> go (Left x : orig) xs 
         EQ -> go (Right (a, x):rs) xs
         GT -> error "The input list to compressSingletons was not sorted."

elevateSuccessors :: (Ord a, Enum a) => RangeMerge a -> RangeMerge a
elevateSuccessors rm = rm
   -- TODO see if you can use the remaining singletons to expand the existing spans
   { spanRanges = newRanges ++ updatedSpans
   , singletons = finalRemaining
   }
   where
      (remainingSingletons, newRanges) = partitionEithers . compressSingletons . singletons $ rm
      (updatedSpans, finalRemaining) = expandSpans (spanRanges rm) remainingSingletons

expandSpans :: (Enum a, Ord a) => [(a, a)] -> [a] -> ([(a, a)], [a])
expandSpans existingSpans values = foldr expanding (existingSpans, []) values 
   where
      expanding :: (Enum a, Ord a) => a -> ([(a, a)], [a]) -> ([(a, a)], [a])
      expanding nextValue (existing, rem) = case tryExtend nextValue existing of
         Just newSpans -> (newSpans, rem)
         Nothing -> (existing, nextValue : rem)

      tryExtend :: (Enum a, Ord a) => a -> [(a, a)] -> Maybe [(a, a)]
      tryExtend value = go
         where
            go [] = Nothing
            go (o@(x, y) : xs) = case extendSpan value o of
               Just newSpan -> Just (newSpan : xs)
               Nothing -> case go xs of
                  Just res -> Just $ o : res
                  Nothing -> Nothing

      extendSpan :: (Enum a, Ord a) => a -> (a, a) -> Maybe (a, a)
      extendSpan x o@(a, b) = if succ x == a 
         then Just (x, b)
         else if succ b == x
            then Just (a, x)
            else Nothing

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
updateBounds rm = updatedSingletons
   where
      updatedSingletons = foldr singletonUpdate (updatedSpans {singletons = []}) (singletons updatedSpans)
      updatedSpans = foldr singleSpanUpdate rmNoSpans (spanRanges rm)

      rmNoSpans = rm { spanRanges = [] }

      singleSpanUpdate :: (Ord a) => (a, a) -> RangeMerge a -> RangeMerge a
      singleSpanUpdate o@(x, y) rm = case (updatedLower, updatedUpper) of
         (Nothing, Nothing) -> rm { spanRanges = o : spanRanges rm }
         (a@(Just _), Nothing) -> rm { largestLowerBound = a }
         (Nothing, b@(Just _)) -> rm { largestUpperBound = b }
         (a, b) -> rm
            { largestLowerBound = a
            , largestUpperBound = b
            }
         where
            updatedLower = update (<= y) (min x) . largestLowerBound $ rm
            updatedUpper = update (x <=) (max y) . largestUpperBound $ rm

      update :: (Ord a) => (a -> Bool) -> (a -> a) -> Maybe a -> Maybe a
      update _ _ Nothing = Nothing
      update compare step (Just value) = if compare value
         then Just $ step value
         else Nothing

      singletonUpdate :: (Ord a, Enum a) => a -> RangeMerge a -> RangeMerge a
      singletonUpdate value rm = case (updatedLower, updatedUpper) of
         (Nothing, Nothing) -> rm { singletons = value : singletons rm }
         (a@(Just _), Nothing) -> rm { largestLowerBound = a }
         (Nothing, b@(Just _)) -> rm { largestUpperBound = b }
         (a, b) -> rm
            { largestLowerBound = a
            , largestUpperBound = b
            }
         where
            updatedLower = update ((succ value) >=) (min value) . largestLowerBound $ rm 
            updatedUpper = update ((pred value) <=) (max value) . largestUpperBound $ rm 

optimizeRangeMerge :: (Ord a, Enum a) => RangeMerge a -> RangeMerge a
optimizeRangeMerge = updateBounds . compressSpans . elevateSuccessors

exportRangeMerge :: (Ord a, Enum a) => RangeMerge a -> [Range a]
exportRangeMerge rm = if isInfRange rm
   then [InfiniteRange]
   else case optimizeRangeMerge rm of
      (RM True _ _ _ _) -> [InfiniteRange]
      orm -> putAll orm
   where
      putAll :: RangeMerge a -> [Range a]
      putAll (RM _ lb up spans singles) = 
         putLowerBound lb ++ putUpperBound up ++ putSpans spans ++ putSingles singles

      putLowerBound = maybe [] (return . LowerBoundRange)
      putUpperBound = maybe [] (return . UpperBoundRange)
      putSpans = map (uncurry SpanRange)
      putSingles = map SingletonRange

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

mergeRange :: (Ord a) => Range a -> Range a -> Either (Range a, Range a) (Range a)
mergeRange r1 r2 = if rangesOverlap r1 r2
                     then Right $ assumeMerge r1 r2
                     else Left (r1, r2)
   where
      assumeMerge :: (Ord a) => Range a -> Range a -> Range a
      assumeMerge (SingletonRange _) x = x
      assumeMerge (SpanRange x y) (SpanRange a b) = SpanRange (min x a) (max y b)
      assumeMerge (SpanRange x _) (LowerBoundRange lower) = LowerBoundRange (min x lower)
      assumeMerge (SpanRange _ y) (UpperBoundRange upper) = UpperBoundRange (max y upper)
      assumeMerge (LowerBoundRange a) (LowerBoundRange b) = LowerBoundRange (min a b)
      assumeMerge (LowerBoundRange _) (UpperBoundRange _) = InfiniteRange
      assumeMerge (UpperBoundRange a) (UpperBoundRange b) = UpperBoundRange (max a b)
      assumeMerge InfiniteRange _ = InfiniteRange
      assumeMerge a b = assumeMerge b a

flipOrdering :: Ordering -> Ordering
flipOrdering LT = GT
flipOrdering GT = LT
flipOrdering EQ = EQ

-- The longer the range the earlier that it should go in the pipeline.
orderRanges :: (Ord a) => Range a -> Range a -> Ordering
orderRanges (SingletonRange a) (SingletonRange b) = compare a b
orderRanges (SingletonRange a) (SpanRange x _) = compare a x
orderRanges _ (LowerBoundRange _) = GT
orderRanges _ (UpperBoundRange _) = GT
orderRanges (SpanRange x _) (SpanRange a _) = compare x a
orderRanges InfiniteRange _ = LT
orderRanges a b = flipOrdering $ orderRanges b a

sortRanges :: (Ord a) => [Range a] -> [Range a]
sortRanges = sortBy orderRanges

-- If you have an infinite range then you can stop there, the merging is complete
mergeRanges :: (Ord a) => [Range a] -> [Range a]
mergeRanges = mergeRangesHelper . sortRanges
   where
      mergeRangesHelper :: (Ord a) => [Range a] -> [Range a]
      mergeRangesHelper (x:y:xs) = case mergeRange x y of
                                     Left (a, b) -> a : mergeRangesHelper (b:xs)
                                     Right a -> mergeRangesHelper (a:xs)
      mergeRangesHelper xs = xs

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
