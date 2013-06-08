module Data.Range.Range (
      Range(..),
      inRange,
      rangesOverlap,
      mergeRange,
      mergeRanges,
      fromRanges,
      fromMergedRanges
   ) where

import Data.Ord (comparing)
import Data.List (sortBy)

data Range a
   = SingletonRange a
   | SpanRange a a
   | InfiniteRange
   deriving(Eq, Show)

infiniteRange :: Range a
infiniteRange = SpanRange Nothing Nothing

rangesOverlap :: (Ord a) => Range a -> Range a -> Bool
-- rangesOverlap InfiniteRange _ = True
-- rangesOverlap _ InfiniteRange = True
rangesOverlap (SingletonRange a) (SingletonRange b) = a == b
rangesOverlap (SingletonRange a) (SpanRange x y) = isBetween a (x, y)
rangesOverlap (SpanRange x y) (SingletonRange a) = isBetween a (x, y)
rangesOverlap (SpanRange Nothing Nothing) _ = True -- Infinite ranges overlap
rangesOverlap _ (SpanRange Nothing Nothing) = True
rangesOverlap (SpanRange _ Nothing) (SpanRange _ Nothing) = True -- Ranges same direction
rangesOverlap (SpanRange Nothing _) (SpanRange Nothing _) = True
rangesOverlap (SpanRange Nothing (Just y)) (SpanRange (Just x) Nothing) = x <= y
rangesOverlap (SpanRange (Just x) Nothing) (SpanRange Nothing (Just y)) = x <= y
rangesOverlap (SpanRange (Just x) (Just y)) (SpanRange (Just a) (Just b)) 
   = isBetween x (Just a, Just b) || isBetween a (Just x, Just y)

-- x- y- will both overlap
-- 3- -9 will overlap
-- x- -y will overlap if x <= y

inRange :: (Ord a) => Range a -> a -> Bool
inRange (SingletonRange a) value = value == a
inRange (SpanRange x y) value = isBetween value (x, y)

isBetween :: (Ord a) => a -> (Maybe a, Maybe a) -> Bool
isBetween a (x, y) = maybe True (<= a) x && maybe True (a <=) y

mergeRange :: (Ord a) => Range a -> Range a -> Either (Range a, Range a) (Range a)
mergeRange r1 r2 = if rangesOverlap r1 r2
                     then Right $ assumeMerge r1 r2
                     else Left (r1, r2)
   where
      assumeMerge :: (Ord a) => Range a -> Range a -> Range a
      assumeMerge (SingletonRange _) x = x
      assumeMerge x (SingletonRange _) = x
      assumeMerge (SpanRange x y) (SpanRange a b) = SpanRange (minMaybe x a) (maxMaybe y b)

      minMaybe :: (Ord a) => Maybe a -> Maybe a -> Maybe a
      minMaybe = min

      maxMaybe :: (Ord a) => Maybe a -> Maybe a -> Maybe a
      maxMaybe Nothing _ = Nothing
      maxMaybe _ Nothing = Nothing
      maxMaybe x y = max x y 

flipOrdering :: Ordering -> Ordering
flipOrdering LT = GT
flipOrdering GT = LT
flipOrdering EQ = EQ

-- The longer the range the earlier that it should go in the pipeline.
orderRanges :: (Ord a) => Range a -> Range a -> Ordering
orderRanges (SingletonRange x) (SingletonRange y) = compare x y
orderRanges _ (SpanRange Nothing _) = GT
orderRanges (SpanRange Nothing _) _ = LT
orderRanges (SingletonRange x) (SpanRange (Just y) _) = compare x y
orderRanges (SpanRange (Just x) _) (SingletonRange y) = compare x y
orderRanges (SpanRange (Just x) _) (SpanRange (Just y) _) = compare x y

sortRanges :: (Ord a) => [Range a] -> [Range a]
sortRanges = sortBy orderRanges

mergeRanges :: (Ord a) => [Range a] -> [Range a]
mergeRanges = mergeRangesHelper . sortRanges
   where
      mergeRangesHelper :: (Ord a) => [Range a] -> [Range a]
      mergeRangesHelper (x:y:xs) = case mergeRange x y of
                                     Left (a, b) -> a : mergeRangesHelper (b:xs)
                                     Right a -> mergeRangesHelper (a:xs)
      mergeRangesHelper xs = xs

fromRanges :: (Ord a, Enum a) => [Range a] -> [a]
fromRanges [] = []
fromRanges (x:xs) = (case x of
   SingletonRange x -> [x] 
   SpanRange Nothing _ -> error "Cannot generate an infinite range."
   SpanRange _ Nothing -> error "Cannot generate an infinite range."
   SpanRange (Just a) (Just b) -> [a..b]
   ) ++ fromRanges xs

fromMergedRanges :: (Ord a, Enum a) => [Range a] -> [a]
fromMergedRanges = fromRanges . mergeRanges
