module Main where

import Test.Framework (defaultMain, testGroup)
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2

import System.Random

import Data.Range.Range


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
prop_infinite_range_contains_everything value = inRange InfiniteRange value

tests_string = testGroup "inRange Function"
   [ testProperty "equal singletons in range" prop_singleton_in_range
   , testProperty "unequal singletons not in range" prop_singleton_not_in_range
   , testProperty "spans contain values in their middles" prop_span_contains
   , testProperty "infinite ranges contain everything" prop_infinite_range_contains_everything
   ]

--tests :: [Test]
tests = [ tests_string ]

main = defaultMain tests
