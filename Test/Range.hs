module Main where

import Test.Framework (defaultMain, testGroup)
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2

import Control.Monad (liftM)
import Data.List
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

data ArbitrarySingletonPair = ASP 

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

-- This property is only true for the invertRM function because the invert function will
-- also get rid of duplicates.
{-
prop_invert_twice_is_identity :: [Range Integer] -> Bool
prop_invert_twice_is_identity x = null (x \\ inverted) && null (inverted \\ x)
   where
      inverted = invert . invert $ x
-}

-- Next idea, the size after two inverts never gets larger

{-
testInvert = testGroup "invert function"
   [ testProperty "inverting twice results in identity" prop_invert_twice_is_identity
   ]
   -}

{-
 - Example properties 
 - prop_union_with_empty_is_self
 - prop_interseciton_with_infinite_is_self
 - prop_demorgans_law when I finally implement a not operation
 -
 - After you do an intersection I want to test that only the bits that are in the
 - intersection are still in range
 -
 - After you do a union I want to test that everything from both ranges is still in range
 -
 - After you perform a not operation I want to confirm that everything that was once in
 - range is no longer in range.
 -
 -}

-- an intersection of a value followed by a union of that value should be the identity.
-- This is false. An intersection of a value followed by a union of that value should be
-- the value itself.
-- (1, 3) union (3, 4) => (1, 4)
-- (1, 3) intersection (3, 4) = (3, 3)
-- ((1, 3) intersection (3, 4)) union (3, 4) => (3, 4)

--tests :: [Test]
tests = 
   [ tests_inRange 
   , test_invertRM
   ]

main = defaultMain tests
