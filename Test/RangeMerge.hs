{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This is only okay in test classes

module Test.RangeMerge
  ( rangeMergeTestCases,
  )
where

import Data.List (subsequences)
import Data.Maybe (fromMaybe)
import Data.Range.Typed.Data
import Data.Range.Typed.RangeInternal
import Data.Range.Typed.Util
import System.Random
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

instance (Num a, Integral a, Ord a, Random a) => Arbitrary (RangeMerge a) where
  shrink = fmap (foldr unionRangeMerges emptyRangeMerge) . init . subsequences . unmergeRM

  arbitrary = do
    upper <- maybeNumber
    possibleSpanStart <- arbitrarySizedIntegral
    spans <- generateSpanList (fromMaybe possibleSpanStart upper)
    lower <-
      oneof
        [ Just . (+) (maxMaybe (boundValue . snd <$> lastMaybe spans) $ maxMaybe upper possibleSpanStart) <$> choose (2, 100),
          return Nothing
        ]
    return
      RM
        { largestUpperBound = InclusiveBound <$> upper,
          largestLowerBound = InclusiveBound <$> lower,
          spanRanges = spans
        }
    where
      maybeNumber = oneof [Just <$> arbitrarySizedIntegral, return Nothing]

      lastMaybe :: [a] -> Maybe a
      lastMaybe [] = Nothing
      lastMaybe xs = Just . last $ xs

      maxMaybe :: (Ord a) => Maybe a -> a -> a
      maxMaybe Nothing x = x
      maxMaybe (Just y) x = max x y

      generateSpanList :: (Num a, Ord a, Random a) => a -> Gen [(Bound a, Bound a)]
      generateSpanList start = do
        count <- choose (0, 10)
        helper count start
        where
          genBound x = oneof [return $ InclusiveBound x, return $ ExclusiveBound x]
          helper :: (Num a, Ord a, Random a) => Integer -> a -> Gen [(Bound a, Bound a)]
          helper 0 _ = return []
          helper x hStart = do
            first <- (+ hStart) <$> choose (2, 100)
            second <- (+ first) <$> choose (2, 100)
            firstBound <- genBound first
            secondBound <- genBound second
            remainder <- helper (x - 1) second
            return $ (firstBound, secondBound) : remainder

prop_export_load_is_identity :: RangeMerge Integer -> Bool
prop_export_load_is_identity x = loadRanges (exportRangeMerge x) == x

test_loadRM :: Test
test_loadRM =
  testGroup
    "loadRanges function"
    [ testProperty "loading export results in identity" prop_export_load_is_identity
    ]

prop_invert_twice_is_identity :: RangeMerge Integer -> Bool
prop_invert_twice_is_identity x = (invertRM . invertRM $ x) == x

test_invertRM :: Test
test_invertRM =
  testGroup
    "invertRM function"
    [ testProperty "inverting twice results in identity" prop_invert_twice_is_identity
    ]

prop_union_with_empty_is_self :: RangeMerge Integer -> Bool
prop_union_with_empty_is_self rm = (rm `unionRangeMerges` emptyRangeMerge) == rm

prop_union_with_infinite_is_infinite :: RangeMerge Integer -> Bool
prop_union_with_infinite_is_infinite rm = (rm `unionRangeMerges` IRM) == IRM

test_unionRM :: Test
test_unionRM =
  testGroup
    "unionRangeMerges function"
    [ testProperty "Union with empty is self" prop_union_with_empty_is_self,
      testProperty "Union with infinite is infinite" prop_union_with_infinite_is_infinite
    ]

prop_intersection_with_empty_is_empty :: RangeMerge Integer -> Bool
prop_intersection_with_empty_is_empty rm =
  (rm `intersectionRangeMerges` emptyRangeMerge) == emptyRangeMerge

prop_intersection_with_infinite_is_self :: RangeMerge Integer -> Bool
prop_intersection_with_infinite_is_self rm =
  (rm `intersectionRangeMerges` IRM) == rm

test_intersectionRM :: Test
test_intersectionRM =
  testGroup
    "intersectionRangeMerges function"
    [ testProperty "Intersection with empty is empty" prop_intersection_with_empty_is_empty,
      testProperty "Intersection with infinite is self" prop_intersection_with_infinite_is_self
    ]

prop_demorgans_law_one :: (RangeMerge Integer, RangeMerge Integer) -> Bool
prop_demorgans_law_one (a, b) =
  invertRM (a `unionRangeMerges` b) == invertRM a `intersectionRangeMerges` invertRM b

prop_demorgans_law_two :: (RangeMerge Integer, RangeMerge Integer) -> Bool
prop_demorgans_law_two (a, b) =
  invertRM (a `intersectionRangeMerges` b) == invertRM a `unionRangeMerges` invertRM b

test_complex_laws :: Test
test_complex_laws =
  testGroup
    "complex set theory rules"
    [ testProperty "DeMorgan Part 1: not (a or b) == (not a) and (not b)" (verboseShrinking (withMaxSuccess 10000 prop_demorgans_law_one)),
      testProperty "DeMorgan Part 2: not (a and b) == (not a) or (not b)" (verboseShrinking (withMaxSuccess 10000 prop_demorgans_law_two))
    ]

rangeMergeTestCases :: [Test]
rangeMergeTestCases =
  [ test_loadRM,
    test_invertRM,
    test_unionRM,
    test_intersectionRM,
    test_complex_laws
  ]
