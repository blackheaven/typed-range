# range - by Robert Massaioli

![build-status](https://bitbucket-badges.atlassian.io/badge/robertmassaioli/range.svg)
![haskell](https://img.shields.io/badge/haskell-safe-green.svg)

The range library is written in Haskell and it's purpose is to make it easy to deal with
ranges. For example you may have the following ranges:

    1-4, 6-23, 15-50, 90-

And you are given a value x, how do you know if the value x is in those ranges? That is
the question that this library answers. You just load your ranges using the library and then
you can query to see if values exist inside your ranges. This library aims to be as
efficient as light as possible while still being useful.

## Example Code

Here is a small example program written using this library:

``` haskell
module Main where

import Data.Range.Range

putStatus :: Bool -> String -> IO ()
putStatus result test = putStrLn $ "[" ++ (show result) ++ "] " ++ test

main = do
    inRanges [SingletonRange 4]   4        `putStatus` "Singletons Match"
    inRanges [SpanRange 0 10]     7        `putStatus` "Value in Range"
    inRanges [LowerBoundRange 80] 12345    `putStatus` "Value in Long Range"
    inRanges [InfiniteRange]      8287423  `putStatus` "Value in Infinite Range"
    inRanges [LowerBoundRange 50, SpanRange 1 30] 44 `putStatus` "NOT in Composite Range (expect false)"
```

If you wish to see a better example in a real program then you should check out [splitter][1].

## Installation Instructions

You can install the range library in the standard way that you install any other Haskell
library: using Cabal. I have uploaded this package to Hackage so you can get it by:

``` shell
cabal install range
```

If you wish to install it from source then check out this repository and do the following:

``` shell
cd /path/to/haskell/range
cabal install
```

You may also wish to work on this library in a development environment, in which case you
should  run:

``` shell
cd /path/to/haskell/range
cabal-dev install
```

And that is all that there is to it. I hope you enjoy using this library and make great
projects with it.

 [1]: http://hackage.haskell.org/package/splitter