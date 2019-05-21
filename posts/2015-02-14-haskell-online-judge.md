---
layout: post
title: "Online Judge Systems accepting Haskell"
date: 2015-02-14 17:30
comments: true
categories: 
---
Online judge system is a good way to hone your problem solving skills and sharp your expressibility in specific languages. Back in school, I was always using C++98 as the main programming language for submission, but purely based on the reason that it is the main stream choice. Now, many years later, with the proliferation of online judge system, many niche or less popular languages are supported as well. And my favorite is Haskell. The problem is that most of the judge system does not keep up the speed of the growth of GHC, with their compilers used for judging not as the latest one. For example, Sphere Online Judge does support Haskell, but it only support ghc-7.6, and without haskell-platform installed.Codeforces has the similar issue since the last time I checked

Good news is, rennovated TIOJ is now support ghc-7.8.3 + Haskell Platform. I picked a dynamic programming problem and was judged without issue. Just it might be because it was run on an older machine, it is bit of slow. Otherwise, it did go very well.

My Program:

```
{-# LANGUAGE OverloadedStrings, MultiWayIf #-}
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as B
import Data.Array.Unboxed
import Data.Array.MArray as MArray
import Data.Array.ST (runSTArray)
import Data.Char (digitToInt)

go :: ByteString -> Int -> Int -> Int
go s n =
  let arr :: Array Int Int
      arr = listArray (0, n) [f i | i <- [0 .. n]]
      f :: Int -> Int
      f i
        | i <= 1 = 1
        | otherwise =
            let c1 = digitToInt $ B.index s (i-2)
                c2 = digitToInt $ B.index s (i-1)
                in if | c2 == 0 -> arr ! (i-2)
                      | (c1 == 1) || (c1 == 2 && c2 >= 1 && c2 <= 6) -> (arr ! (i-1)) + (arr ! (i-2))
                      | otherwise -> arr ! (i-1)
  in \x -> arr ! x

solve :: ByteString -> Int
solve s =
  let l = B.length s
  in (go s l)  l

main = runMaybeT $ forever $ do
  s <- lift B.getLine
  when (s == “0”) $ mzero
  let sol = solve s
  lift $ print sol
```
