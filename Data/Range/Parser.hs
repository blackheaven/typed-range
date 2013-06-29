{-# LANGUAGE FlexibleContexts #-}

module Data.Range.Parser 
   ( parseRanges
   , ranges
   , RangeParserArgs(..)
   , defaultArgs
   ) where

import Text.Parsec
import Text.Parsec.String

import Data.Range.Range

{-
 - I want to be able to parse the following formats into ranges
 - * Single numbers
 - * Spans with separating characters
 - * Infinite wildcard ranges
 - * Infinite in one direction ranges
 -
 - Then I want to be able to parse arbitrary ranges.
 -}

data RangeParserArgs = Args 
   { unionSeparator :: String
   , intersectionSeparator :: String
   , rangeSeparator :: String
   , wildcardSymbol :: String
   }
   deriving(Show)

-- Break them into token first with Alex and then parse them into ranges with Happy. Make
-- it so that Alex is configurable and different characters can be used as tokens. I
-- wonder if it is even possible to do this in a smart way? If we cannot do this then use
-- parsec. Maybe it would be better to use parsec from the start.

defaultArgs :: RangeParserArgs 
defaultArgs = Args
   { unionSeparator = ","
   , rangeSeparator = "-"
   , intersectionSeparator = "&" -- this one seems wrong, work on it
   , wildcardSymbol = "*"
   }

parseRanges :: (Read a) => String -> Either ParseError [Range a]
parseRanges = parse (ranges defaultArgs) "(range parser)"

string_ :: Stream s m Char => String -> ParsecT s u m ()
string_ x = string x >> return ()

ranges :: (Read a) => RangeParserArgs -> Parser [Range a]
ranges args = range `sepBy` (string $ unionSeparator args)
   where 
      range :: (Read a) => Parser (Range a)
      range = choice 
         [ infiniteRange
         , spanRange
         , singletonRange
         ]

      infiniteRange :: (Read a) => Parser (Range a)
      infiniteRange = do
         string_ $ wildcardSymbol args
         return InfiniteRange

      spanRange :: (Read a) => Parser (Range a)
      spanRange = try $ do
         first <- readSection
         string_ $ rangeSeparator args
         second <- readSection
         case (first, second) of
            (Just x, Just y)  -> return $ SpanRange x y
            (Just x, _)       -> return $ LowerBoundRange x
            (_, Just y)       -> return $ UpperBoundRange y
            _                 -> parserFail ("Range should have a number on one end: " ++ rangeSeparator args)

      singletonRange :: (Read a) => Parser (Range a)
      singletonRange = fmap (SingletonRange . read) $ many1 digit

readSection :: (Read a) => Parser (Maybe a)
readSection = fmap (fmap read) $ optionMaybe (many1 digit)
