{-# LANGUAGE LambdaCase #-}

module Data.Range.Typed.RangeInternal where

import Control.Monad (guard)
import Data.Functor (($>))
import Data.Maybe (catMaybes, mapMaybe)
import Data.Range.Typed.Data
import Data.Range.Typed.Spans
import Data.Range.Typed.Util

{-
 - The following assumptions must be maintained at the beginning of these internal
 - functions so that we can reason about what we are given.
 -
 - RangeMerge assumptions:
 - * The span ranges will never overlap the bounds.
 - * The span ranges are always sorted in ascending order by the first element.
 - * The lower and upper bounds never overlap in such a way to make it an infinite range.
 -}
data RangeMerge a
  = RM
      { largestLowerBound :: Maybe (Bound a),
        largestUpperBound :: Maybe (Bound a),
        spanRanges :: [(Bound a, Bound a)]
      }
  | IRM
  | ERM
  deriving (Show, Eq)

emptyRangeMerge :: RangeMerge a
emptyRangeMerge = RM Nothing Nothing []

storeRange :: (Ord a) => AnyRangeFor c a -> RangeMerge a
storeRange (AnyRangeFor range) =
  case range of
    InfiniteRange -> IRM
    EmptyRange -> ERM
    LowerBoundRange lower -> emptyRangeMerge {largestLowerBound = Just lower}
    UpperBoundRange upper -> emptyRangeMerge {largestUpperBound = Just upper}
    SpanRange x y
      | boundValue x == boundValue y && pointJoinType x y == Separate -> emptyRangeMerge
      | otherwise -> emptyRangeMerge {spanRanges = [(minBounds x y, maxBounds x y)]}
    SingletonRange x -> emptyRangeMerge {spanRanges = [(InclusiveBound x, InclusiveBound x)]}

storeRanges :: (Ord a) => RangeMerge a -> [AnyRangeFor c a] -> RangeMerge a
storeRanges = foldr (unionRangeMerges . storeRange)

loadRanges :: (Ord a) => [AnyRangeFor c a] -> RangeMerge a
loadRanges = storeRanges emptyRangeMerge
{-# INLINE [0] loadRanges #-}

exportRangeMerge :: (Eq a) => RangeMerge a -> [AnyRange a]
exportRangeMerge =
  \case
    IRM -> [AnyRangeFor InfiniteRange]
    ERM -> [AnyRangeFor EmptyRange]
    RM lb up spans ->
      let putLowerBound :: Maybe (Bound a) -> [AnyRange a]
          putLowerBound = maybe [] (return . AnyRangeFor . LowerBoundRange)
          putUpperBound :: Maybe (Bound a) -> [AnyRange a]
          putUpperBound = maybe [] (return . AnyRangeFor . UpperBoundRange)
          putSpans = map simplifySpan
          simplifySpan (x, y) =
            if x == y && pointJoinType x y /= Separate
              then AnyRangeFor $ SingletonRange $ boundValue x
              else AnyRangeFor $ SpanRange x y
       in putUpperBound up <> putSpans spans <> putLowerBound lb

{-# RULES "load/export" [1] forall x. loadRanges (exportRangeMerge x) = x #-}

intersectSpansRM :: (Ord a) => RangeMerge a -> RangeMerge a -> RangeMerge a
intersectSpansRM one two = RM Nothing Nothing newSpans
  where
    newSpans = intersectSpans (spanRanges one) (spanRanges two)

intersectWith :: (Ord a) => (Bound a -> (Bound a, Bound a) -> Maybe (Bound a, Bound a)) -> Maybe (Bound a) -> [(Bound a, Bound a)] -> [(Bound a, Bound a)]
intersectWith _ Nothing _ = []
intersectWith fix (Just lower) xs = mapMaybe (fix lower) xs

fixLower :: (Ord a) => Bound a -> (Bound a, Bound a) -> Maybe (Bound a, Bound a)
fixLower lower (x, y) = do
  guard (boundValue lower <= boundValue y)
  return (maxBoundsIntersection lower x, y)

fixUpper :: (Ord a) => Bound a -> (Bound a, Bound a) -> Maybe (Bound a, Bound a)
fixUpper upper (x, y) = do
  guard (boundValue x <= boundValue upper)
  return (x, minBoundsIntersection y upper)

intersectionRangeMerges :: (Ord a) => RangeMerge a -> RangeMerge a -> RangeMerge a
intersectionRangeMerges ERM _ = ERM
intersectionRangeMerges _ ERM = ERM
intersectionRangeMerges IRM two = two
intersectionRangeMerges one IRM = one
intersectionRangeMerges one two =
  RM
    { largestLowerBound = newLowerBound,
      largestUpperBound = newUpperBound,
      spanRanges = unionSpans sortedResults
    }
  where
    lowerOneSpans = intersectWith fixLower (largestLowerBound one) (spanRanges two)
    lowerTwoSpans = intersectWith fixLower (largestLowerBound two) (spanRanges one)
    upperOneSpans = intersectWith fixUpper (largestUpperBound one) (spanRanges two)
    upperTwoSpans = intersectWith fixUpper (largestUpperBound two) (spanRanges one)
    intersectedSpans = intersectSpans (spanRanges one) (spanRanges two)

    sortedResults =
      removeEmptySpans $
        foldr1
          insertionSortSpans
          [ lowerOneSpans,
            lowerTwoSpans,
            upperOneSpans,
            upperTwoSpans,
            intersectedSpans,
            calculateBoundOverlap one two
          ]

    newLowerBound = calculateNewBound largestLowerBound maxBoundsIntersection one two
    newUpperBound = calculateNewBound largestUpperBound minBoundsIntersection one two

    calculateNewBound ::
      (Ord a) =>
      (RangeMerge a -> Maybe (Bound a)) ->
      (Bound a -> Bound a -> Bound a) ->
      RangeMerge a ->
      RangeMerge a ->
      Maybe (Bound a)
    calculateNewBound ext comp one' two' = case (ext one', ext two') of
      (Just x, Just y) -> Just $ comp x y
      (_, Nothing) -> Nothing
      (Nothing, _) -> Nothing

calculateBoundOverlap :: (Ord a) => RangeMerge a -> RangeMerge a -> [(Bound a, Bound a)]
calculateBoundOverlap one two = catMaybes [oneWay, secondWay]
  where
    oneWay = do
      x <- largestLowerBound one
      y <- largestUpperBound two
      guard (compareLower y x /= LT)
      return (x, y)

    secondWay = do
      x <- largestLowerBound two
      y <- largestUpperBound one
      guard (compareLower y x /= LT)
      return (x, y)

unionRangeMerges :: (Ord a) => RangeMerge a -> RangeMerge a -> RangeMerge a
unionRangeMerges ERM one = one
unionRangeMerges one ERM = one
unionRangeMerges IRM _ = IRM
unionRangeMerges _ IRM = IRM
unionRangeMerges one two = infiniteCheck filterTwo
  where
    filterOne = foldr filterLowerBound boundedRM (unionSpans sortedSpans)
    filterTwo = foldr filterUpperBound (filterOne {spanRanges = []}) (spanRanges filterOne)

    infiniteCheck :: (Ord a) => RangeMerge a -> RangeMerge a
    infiniteCheck IRM = IRM
    infiniteCheck rm@(RM (Just lower) (Just upper) _) =
      if compareUpperToLower upper lower /= LT
        then IRM
        else rm
    infiniteCheck rm = rm

    newLowerBound = calculateNewBound largestLowerBound minBounds one two
    newUpperBound = calculateNewBound largestUpperBound maxBounds one two

    sortedSpans = insertionSortSpans (spanRanges one) (spanRanges two)

    boundedRM =
      RM
        { largestLowerBound = newLowerBound,
          largestUpperBound = newUpperBound,
          spanRanges = []
        }

    calculateNewBound ::
      (Ord a) =>
      (RangeMerge a -> Maybe (Bound a)) ->
      (Bound a -> Bound a -> Bound a) ->
      RangeMerge a ->
      RangeMerge a ->
      Maybe (Bound a)
    calculateNewBound ext comp one' two' = case (ext one', ext two') of
      (Just x, Just y) -> Just $ comp x y
      (z, Nothing) -> z
      (Nothing, z) -> z

filterLowerBound :: (Ord a) => (Bound a, Bound a) -> RangeMerge a -> RangeMerge a
filterLowerBound _ ERM = ERM
filterLowerBound _ IRM = IRM
filterLowerBound a rm@(RM Nothing _ _) = rm {spanRanges = a : spanRanges rm}
filterLowerBound s@(lower, _) rm@(RM (Just lowestBound) _ _) =
  case boundCmp lowestBound s of
    GT -> rm {spanRanges = s : spanRanges rm}
    LT -> rm
    EQ -> rm {largestLowerBound = Just $ minBounds lowestBound lower}

filterUpperBound :: (Ord a) => (Bound a, Bound a) -> RangeMerge a -> RangeMerge a
filterUpperBound _ ERM = ERM
filterUpperBound _ IRM = IRM
filterUpperBound a rm@(RM _ Nothing _) = rm {spanRanges = a : spanRanges rm}
filterUpperBound s@(_, upper) rm@(RM _ (Just upperBound') _) =
  case boundCmp upperBound' s of
    LT -> rm {spanRanges = s : spanRanges rm}
    GT -> rm
    EQ -> rm {largestUpperBound = Just $ maxBounds upperBound' upper}

invertRM :: (Ord a) => RangeMerge a -> RangeMerge a
invertRM ERM = IRM
invertRM IRM = emptyRangeMerge
invertRM (RM Nothing Nothing []) = IRM
invertRM (RM (Just lower) Nothing []) = RM Nothing (Just . invertBound $ lower) []
invertRM (RM Nothing (Just upper) []) = RM (Just . invertBound $ upper) Nothing []
invertRM (RM (Just lower) (Just upper) []) = RM Nothing Nothing [(invertBound upper, invertBound lower)]
invertRM rm =
  RM
    { largestUpperBound = newUpperBound,
      largestLowerBound = newLowerBound,
      spanRanges = upperSpan <> betweenSpans <> lowerSpan
    }
  where
    newLowerValue = invertBound $ snd $ last $ spanRanges rm
    newUpperValue = invertBound $ fst $ head $ spanRanges rm

    newUpperBound = case largestUpperBound rm of
      Just _ -> Nothing
      Nothing -> Just newUpperValue

    newLowerBound = case largestLowerBound rm of
      Just _ -> Nothing
      Nothing -> Just newLowerValue

    upperSpan = case largestUpperBound rm of
      Nothing -> []
      Just upper -> [(invertBound upper, newUpperValue)]
    lowerSpan = case largestLowerBound rm of
      Nothing -> []
      Just lower -> [(newLowerValue, invertBound lower)]

    betweenSpans = invertSpans $ spanRanges rm

joinRM :: (Eq a, Enum a) => RangeMerge a -> RangeMerge a
joinRM o@(RM _ _ []) = o
joinRM rm = RM lower higher spansAfterHigher
  where
    joinedSpans = joinSpans $ spanRanges rm

    (lower, spansAfterLower) =
      case (largestLowerBound rm, reverse joinedSpans) of
        o@(Just l, (xl, xh) : xs) ->
          if succ (highestValueInUpperBound xh) == lowestValueInLowerBound l
            then (Just xl, reverse xs)
            else o
        x -> x

    (higher, spansAfterHigher) =
      case (largestUpperBound rm, spansAfterLower) of
        o@(Just h, (xl, xh) : xs) ->
          if highestValueInUpperBound h == pred (lowestValueInLowerBound xl)
            then (Just xh, xs)
            else o
        x -> x

updateBound :: Bound a -> a -> Bound a
updateBound = ($>)

unmergeRM :: RangeMerge a -> [RangeMerge a]
unmergeRM ERM = [ERM]
unmergeRM IRM = [IRM]
unmergeRM (RM lower upper spans) =
  maybe [] (\x -> [RM Nothing (Just x) []]) upper
    <> fmap (\x -> RM Nothing Nothing [x]) spans
    <> maybe [] (\x -> [RM (Just x) Nothing []]) lower
