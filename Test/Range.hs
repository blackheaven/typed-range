{-# OPTIONS_GHC -fno-warn-orphans #-}
-- This is only okay in test classes

module Main where

import Test.Framework (defaultMain, testGroup)
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2

import Control.Monad (liftM)
import System.Random

import Data.Range.Range

import Test.RangeMerge

data UnequalPair a = UnequalPair (a, a)
   deriving (Show)

instance (Num a, Eq a) => Arbitrary (UnequalPair a) where
   arbitrary = do
      first <- arbitrarySizedIntegral
      second <- arbitrarySizedIntegral `suchThat` (/= first)
      return $ UnequalPair (first, second)

prop_singleton_in_range :: Integer -> Bool
prop_singleton_in_range a = inRange (SingletonRange a) a

prop_singleton_not_in_range :: (Ord a) => UnequalPair a -> Bool
prop_singleton_not_in_range (UnequalPair (first, second)) = not $ inRange (SingletonRange first) second

data SpanContains a = SpanContains (a, a) a
   deriving (Show)

instance (Num a, Ord a, Random a) => Arbitrary (SpanContains a) where
   arbitrary = do
      begin <- arbitrarySizedIntegral 
      end <- arbitrarySizedIntegral `suchThat` (>= begin)
      middle <- choose (begin, end)
      return $ SpanContains (begin, end) middle

prop_span_contains :: SpanContains Integer -> Bool
prop_span_contains (SpanContains (begin, end) middle) = inRange (SpanRange begin end) middle

prop_infinite_range_contains_everything :: Integer -> Bool
prop_infinite_range_contains_everything = inRange InfiniteRange

tests_inRange = testGroup "inRange Function"
   [ testProperty "equal singletons in range" prop_singleton_in_range
   , testProperty "unequal singletons not in range" prop_singleton_not_in_range
   , testProperty "spans contain values in their middles" prop_span_contains
   , testProperty "infinite ranges contain everything" prop_infinite_range_contains_everything
   ]

instance (Num a, Ord a, Enum a) => Arbitrary (Range a) where
   arbitrary = oneof 
      [ generateSingleton
      , generateSpan
      , generateLowerBound
      , generateUpperBound
      , generateInfiniteRange
      ]
      where
         generateSingleton = liftM SingletonRange arbitrarySizedIntegral
         generateSpan = do
            first <- arbitrarySizedIntegral 
            second <- arbitrarySizedIntegral `suchThat` (> first)
            return $ SpanRange first second
         generateLowerBound = liftM LowerBoundRange arbitrarySizedIntegral
         generateUpperBound = liftM UpperBoundRange arbitrarySizedIntegral
         generateInfiniteRange :: Gen (Range a)
         generateInfiniteRange = return InfiniteRange

-- an intersection of a value followed by a union of that value should be the identity.
-- This is false. An intersection of a value followed by a union of that value should be
-- the value itself.
-- (1, 3) union (3, 4) => (1, 4)
-- (1, 3) intersection (3, 4) = (3, 3)
-- ((1, 3) intersection (3, 4)) union (3, 4) => (3, 4)

prop_in_range_out_of_range_after_invert :: (Integer, [Range Integer]) -> Bool
prop_in_range_out_of_range_after_invert (point, ranges) = 
   (inRanges ranges point) /= (inRanges (invert ranges) point)

test_ranges_invert = testGroup "invert function for ranges"
   [ testProperty "element in range is now out of range after invert" prop_in_range_out_of_range_after_invert
   ]

prop_elements_before_union_or_true :: ([Integer], [Range Integer], [Range Integer]) -> Bool
prop_elements_before_union_or_true (points, a, b) = actual == expected
   where
      before_a = map (inRanges a) points
      before_b = map (inRanges b) points
      unionRanges = a `union` b
      actual = map (inRanges unionRanges) points
      expected = zipWith (||) before_a before_b

prop_elements_before_intersection_and_true :: ([Integer], [Range Integer], [Range Integer]) -> Bool
prop_elements_before_intersection_and_true (points, a, b) = actual == expected
   where
      before_a = map (inRanges a) points
      before_b = map (inRanges b) points
      intersectedRanges = a `intersection` b
      actual = map (inRanges intersectedRanges) points
      expected = zipWith (&&) before_a before_b

prop_elements_before_difference_and_not_true :: ([Integer], [Range Integer], [Range Integer]) -> Bool
prop_elements_before_difference_and_not_true (points, a, b) = actual == expected
   where
      before_a = map (inRanges a) points
      before_b = map (inRanges b) points
      diffedRanges = a `difference` b
      actual = map (inRanges diffedRanges) points
      expected = zipWith (&&) before_a $ map not before_b

test_union = testGroup "union function properties"
   [ testProperty "Unions from before OR together and continue to work" prop_elements_before_union_or_true
   ]

test_intersection = testGroup "intersection function properties"
   [ testProperty "Intersection before AND's to after" prop_elements_before_intersection_and_true
   ]

test_difference = testGroup "difference function properties"
   [ testProperty "Difference before AND/NOT's to after" prop_elements_before_difference_and_not_true
   ]

--tests :: [Test]
tests = 
   [ tests_inRange 
   , test_ranges_invert
   , test_union
   , test_intersection
   , test_difference
   ]
   ++ rangeMergeTestCases

main = defaultMain tests
