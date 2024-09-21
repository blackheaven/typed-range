# typed-range

The typed-range library is written in Haskell, and its purpose is to make it easy to deal with
ranges. For example, you may have the following ranges:

    1-4, 6-23, 15-50, 90-

And you are given a value x, how do you know if the value x is in those ranges? That is
the question that this library answers. You just load your ranges using the library, and then
you can query to see if values exist inside your ranges. This library aims to be as
efficient as light as possible while still being useful.

## Example Code

Here is a small example program written using this library:

``` haskell
module Main where

import Data.Range.Typed.Range

putStatus :: Bool -> String -> IO ()
putStatus result test = putStrLn $ "[" ++ (show result) ++ "] " ++ test

main = do
    inRanges [anyRange $ SingletonRange 4]   4                         `putStatus` "Singletons Match"
    inRanges [anyRange $ 0 +=+ 10] 7                                   `putStatus` "Value in Range"
    inRanges [anyRange $ LowerBoundRange (Bound 80 Inclusive)] 12345   `putStatus` "Value in Long Range"
    inRanges [anyRange $ InfiniteRange]      8287423                   `putStatus` "Value in Infinite Range"
    inRanges [anyRange $ lbi 50, anyRange $ 1 +=+ 30] 44               `putStatus` "NOT in Composite Range (expect false)"
    inRanges [anyRange $ EmptyRange]      8287423                      `putStatus` "NOT in Infinite Range (expect false)"
```
