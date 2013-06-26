module Test.RangeMerge (
   test_invertRM
   ) where

import Test.Framework (defaultMain, testGroup)
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)
import System.Random

import Data.Range.RangeInternal

instance (Num a, Ord a, Random a) => Arbitrary (RangeMerge a) where
   arbitrary = do
      upperBound <- maybeNumber
      possibleSpanStart <- arbitrarySizedIntegral
      spans <- generateSpanList (fromMaybe possibleSpanStart upperBound)
      possibleLower <- arbitrarySizedIntegral
      lowerBound <- oneof 
         [ fmap Just $ fmap ((+) $ maxMaybe (fmap snd $ lastMaybe spans) $ maxMaybe upperBound possibleSpanStart) $ choose (2, 100)
         , return Nothing
         ]
      return RM 
         { largestUpperBound = upperBound
         , largestLowerBound = lowerBound 
         , spanRanges = spans
         }
      where
         maybeNumber = oneof [liftM Just arbitrarySizedIntegral, return Nothing]

         lastMaybe :: [a] -> Maybe a
         lastMaybe [] = Nothing
         lastMaybe xs = Just . last $ xs

         maxMaybe :: Ord a => Maybe a -> a -> a
         maxMaybe Nothing x = x
         maxMaybe (Just y) x = max x y

         generateSpanList :: (Num a, Ord a, Random a) => a -> Gen [(a, a)]
         generateSpanList start = do
            count <- choose (0, 10)
            helper count start
            where
               helper :: (Num a, Ord a, Random a) => Integer -> a -> Gen [(a, a)]
               helper 0 _ = return []
               helper x start = do
                  first <- fmap (+start) $ choose (2, 100)
                  second <- fmap (+first) $ choose (2, 100)
                  remainder <- helper (x - 1) second
                  return $ (first, second) : remainder

         biggerThan :: Ord a => a -> Maybe a -> Bool
         biggerThan _ Nothing = True
         biggerThan x (Just y) = x > y 

prop_invert_twice_is_identity :: RangeMerge Integer -> Bool
prop_invert_twice_is_identity x = (invertRM . invertRM $ x) == x

prop_demorgans_law_one :: (RangeMerge Integer, RangeMerge Integer) -> Bool
prop_demorgans_law_one (a, b) = 
   (invertRM (a `unionRangeMerges` b)) == ((invertRM a) `intersectionRangeMerges` (invertRM b))

prop_demorgans_law_two :: (RangeMerge Integer, RangeMerge Integer) -> Bool
prop_demorgans_law_two (a, b) = 
   (invertRM (a `intersectionRangeMerges` b)) == ((invertRM a) `unionRangeMerges` (invertRM b))

test_invertRM = testGroup "invertRM function"
   [ testProperty "inverting twice results in identity" prop_invert_twice_is_identity
   , testProperty "DeMorgan Part 1: not (a or b) == (not a) and (not b)" prop_demorgans_law_one
   , testProperty "DeMorgan Part 2: not (a and b) == (not a) or (not b)" prop_demorgans_law_two
   ]
