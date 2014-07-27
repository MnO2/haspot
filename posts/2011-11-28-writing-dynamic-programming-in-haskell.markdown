---
layout: post
title: Writing Dynamic Programming in Haskell
date: 2011-11-28 04:18
comments: true
categories: haskell 
---

From the fact that the function is the composing unit in haskell,
you can quickly guess that the most intuitive way to implement dynamic programming is through memoization.
Of course you can implement through state modification if you insist,
but that is just coding a C program in haskell.

The core of dynamic programming is recursion, but with the exploitation of overlapping subproblem.
Such that we can add another layer of data structure to memorize the previosuly calculated subproblem results.
The power of haskell lies at you can just implement the brute force recursive version, then with little modification it just provides you the semantics of dynamic programming.
There exists two most used library on hackage. 
They are [data-memocombinators](http://hackage.haskell.org/package/data-memocombinators), and [MemoTrie](http://hackage.haskell.org/package/MemoTrie).
Both uses the similar internal mechanism.
MemoTrie is easier to use if you don't need to memorize your own data type, data-memocombinators satisfies your need if you really want that.

The following lists six implementations of [problem31 of project euler](http://projecteuler.net/problem=31) in haskell,
one uses data-memocombinators, one uses MemoTrie, another from HaskellWiki exploits lazy evaluation,
one uses immutable array to implement that. The rest two are from stackoverflow provided by John Lato.

```
import qualified Data.MemoCombinators as Memo

coins = [1,2,5,10,20,50,100,200]

sol1 = f
    where f :: Int -> Int -> Int
          f = Memo.memo2 Memo.integral (Memo.arrayRange (0,7)) mf
            where mf :: Int -> Int -> Int
                  mf n k | (k >= 8) || (n < 0) = 0
                         | n == 0 = 1
                         | otherwise = (f n (k+1)) + (f (n - coins !! k) k)

main = do
    print $ sol1 200 0
```

```
./sol01 +RTS -sstderr 
       4,901,540 bytes allocated in the heap
         220,696 bytes copied during GC
          82,112 bytes maximum residency (1 sample(s))
          22,864 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)

  Generation 0:     9 collections,     0 parallel,  0.00s,  0.00s elapsed
  Generation 1:     1 collections,     0 parallel,  0.00s,  0.00s elapsed

  INIT  time    0.00s  (  0.00s elapsed)
  MUT   time    0.01s  (  0.01s elapsed)
  GC    time    0.00s  (  0.00s elapsed)
  RP    time    0.00s  (  0.00s elapsed)
  PROF  time    0.00s  (  0.00s elapsed)
  EXIT  time    0.00s  (  0.00s elapsed)
  Total time    0.01s  (  0.01s elapsed)

  %GC time       6.9%  (7.3% elapsed)

  Alloc rate    389,289,174 bytes per MUT second

  Productivity  85.8% of total user, 91.4% of total elapsed
```

```
coins = [1,2,5,10,20,50,100,200]

sol2 = mf
    where memo :: (Num a, Enum a) => (a -> b) -> [b]
          memo f = map f (enumFrom 0)
          mf :: Int -> Int -> Int
          mf = \n k -> fvalue !! n !! k
          fvalue = fmap memo (memo f)
          f :: Int -> Int -> Int
          f n k | (k >= 8) || (n < 0) = 0
                | n == 0 = 1
                | otherwise = (if k < 7 then mf n (k+1) else 0) + (if n - coins!!k >= 0 then mf (n - coins !! k) k else 0)

main = do
    print $ sol2 200 0
```

```
./sol02 +RTS -sstderr 
         334,684 bytes allocated in the heap
           2,096 bytes copied during GC
          43,012 bytes maximum residency (1 sample(s))
          26,620 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)

  Generation 0:     0 collections,     0 parallel,  0.00s,  0.00s elapsed
  Generation 1:     1 collections,     0 parallel,  0.00s,  0.00s elapsed

  INIT  time    0.00s  (  0.00s elapsed)
  MUT   time    0.00s  (  0.00s elapsed)
  GC    time    0.00s  (  0.00s elapsed)
  RP    time    0.00s  (  0.00s elapsed)
  PROF  time    0.00s  (  0.00s elapsed)
  EXIT  time    0.00s  (  0.00s elapsed)
  Total time    0.01s  (  0.01s elapsed)

  %GC time       1.9%  (2.3% elapsed)

  Alloc rate    57,142,564 bytes per MUT second

  Productivity  80.1% of total user, 94.6% of total elapsed
```



```
coins = [1,2,5,10,20,50,100,200]

sol3 = (!!) (ways [1,2,5,10,20,50,100,200])
    where ways [] = 1 : repeat 0
          ways (coin:coins) = n
              where n = zipWith (+) (ways coins) (replicate coin 0 ++ n)

main = do
    print $ sol3 200
```

```
./sol03 +RTS -sstderr 
         248,052 bytes allocated in the heap
           2,096 bytes copied during GC
          43,012 bytes maximum residency (1 sample(s))
          26,620 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)

  Generation 0:     0 collections,     0 parallel,  0.00s,  0.00s elapsed
  Generation 1:     1 collections,     0 parallel,  0.00s,  0.00s elapsed

  INIT  time    0.00s  (  0.00s elapsed)
  MUT   time    0.00s  (  0.00s elapsed)
  GC    time    0.00s  (  0.00s elapsed)
  RP    time    0.00s  (  0.00s elapsed)
  PROF  time    0.00s  (  0.00s elapsed)
  EXIT  time    0.00s  (  0.00s elapsed)
  Total time    0.00s  (  0.00s elapsed)

  %GC time       3.2%  (4.0% elapsed)

  Alloc rate    66,041,533 bytes per MUT second

  Productivity  72.3% of total user, 91.1% of total elapsed
```


```
import qualified Data.MemoTrie as MT

coins = [1,2,5,10,20,50,100,200]

sol4 = f
    where f :: Int -> Int -> Int
          f = MT.memo2 mf
            where mf :: Int -> Int -> Int
                  mf n k | (k >= 8) || (n < 0) = 0
                         | n == 0 = 1
                         | otherwise = (f n (k+1)) + (f (n - coins !! k) k)

main = do
    print $ sol4 200 0
```

```
./sol04 +RTS -sstderr 
       3,992,108 bytes allocated in the heap
         874,084 bytes copied during GC
         185,192 bytes maximum residency (1 sample(s))
          26,680 bytes maximum slop
               2 MB total memory in use (0 MB lost due to fragmentation)

  Generation 0:     7 collections,     0 parallel,  0.00s,  0.00s elapsed
  Generation 1:     1 collections,     0 parallel,  0.00s,  0.00s elapsed

  INIT  time    0.00s  (  0.00s elapsed)
  MUT   time    0.01s  (  0.01s elapsed)
  GC    time    0.00s  (  0.00s elapsed)
  RP    time    0.00s  (  0.00s elapsed)
  PROF  time    0.00s  (  0.00s elapsed)
  EXIT  time    0.00s  (  0.00s elapsed)
  Total time    0.01s  (  0.01s elapsed)

  %GC time      22.3%  (24.8% elapsed)

  Alloc rate    505,650,158 bytes per MUT second

  Productivity  66.4% of total user, 73.3% of total elapsed
```



```
import Data.Maybe
import qualified Data.Map as M


coins = [1,2,5,10,20,50,100,200]

sol5 = mf
    where memo :: (Num a, Enum a) => (a -> b) -> [b]
          memo f = map f (enumFrom 0)
          gwvals = fmap memo (memo f)
          gwByMap :: Int -> Int -> Int -> Int -> Int
          gwByMap maxX maxY = \x y ->  fromMaybe (f x y) $ M.lookup (x,y) memomap
            where memomap = M.fromList $ concat [[((x',y'), z) | (y',z) <- zip [0..maxY] ys] | (x',ys) <- zip [0..maxX] gwvals]
          mf :: Int -> Int -> Int
          mf = gwByMap 205 8
          f :: Int -> Int -> Int
          f n k | (k >= 8) || (n < 0) = 0
                | n == 0 = 1
                | otherwise = (if k < 7 then mf n (k+1) else 0) + (if n - coins!!k >= 0 then mf (n - coins !! k) k else 0)

main = do
    print $ sol5 200 0
```

```
./sol05 +RTS -sstderr 
       1,856,632 bytes allocated in the heap
         265,496 bytes copied during GC
          87,788 bytes maximum residency (1 sample(s))
          22,236 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)

  Generation 0:     3 collections,     0 parallel,  0.00s,  0.00s elapsed
  Generation 1:     1 collections,     0 parallel,  0.00s,  0.00s elapsed

  INIT  time    0.00s  (  0.00s elapsed)
  MUT   time    0.01s  (  0.01s elapsed)
  GC    time    0.00s  (  0.00s elapsed)
  RP    time    0.00s  (  0.00s elapsed)
  PROF  time    0.00s  (  0.00s elapsed)
  EXIT  time    0.00s  (  0.00s elapsed)
  Total time    0.01s  (  0.01s elapsed)

  %GC time       8.0%  (8.7% elapsed)

  Alloc rate    197,011,035 bytes per MUT second

  Productivity  82.3% of total user, 89.4% of total elapsed
```

```
import Data.Array.IArray

coins = [1,2,5,10,20,50,100,200]

sol6 = ans
    where ans :: Int -> Int -> Int
          ans n k = table ! (n, k)
            where table :: Array (Int, Int) Int
                  table = listArray ((0,0), (300,7)) [f i j | i <- [0..n], j <- [0..7]]
                  f n k | (k >= 8) || (n < 0) = 0
                        | n == 0 = 1
                        | otherwise = (if k < 7 then table ! (n, (k+1)) else 0) + (if (n - coins!!k) >= 0 then table ! ((n - coins !! k), k) else 0)

main = do
    print $ sol6 200 0
```

```
./sol06 +RTS -sstderr 
         280,488 bytes allocated in the heap
           2,096 bytes copied during GC
          43,012 bytes maximum residency (1 sample(s))
          26,620 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)

  Generation 0:     0 collections,     0 parallel,  0.00s,  0.00s elapsed
  Generation 1:     1 collections,     0 parallel,  0.00s,  0.00s elapsed

  INIT  time    0.00s  (  0.00s elapsed)
  MUT   time    0.00s  (  0.00s elapsed)
  GC    time    0.00s  (  0.00s elapsed)
  RP    time    0.00s  (  0.00s elapsed)
  PROF  time    0.00s  (  0.00s elapsed)
  EXIT  time    0.00s  (  0.00s elapsed)
  Total time    0.01s  (  0.00s elapsed)

  %GC time       2.4%  (2.9% elapsed)

  Alloc rate    51,674,281 bytes per MUT second

  Productivity  78.1% of total user, 93.7% of total elapsed
```

One thing to notice is these two are not equivalent.
```
m1      = ((filter odd [1..]) !!)

m2 n    = ((filter odd [1..]) !! n)
```
This problem is related to [Eta Expansion](http://blog-mno2.csie.org/blog/2012/01/17/floated-out-and-eta-expansion/).
You could probably re-evaluate a lamda function without caution.
Or just checkout the discussion on stackoverflow.

* [What is the lifetime of a memoized value in a functional language like Haskell?](http://stackoverflow.com/questions/5898162/what-is-the-lifetime-of-a-memoized-value-in-a-functional-language-like-haskell)
* [When is memoization automatic in GHC Haskell?](http://stackoverflow.com/questions/3951012/when-is-memoization-automatic-in-ghc-haskell)
* [Haskell function definition and caching arrays](http://stackoverflow.com/questions/307287/haskell-function-definition-and-caching-arrays)
