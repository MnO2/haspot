---
layout: post
title: "Keeping An Eye Out for the Difference between Machine Word and Fixed Word in Haskell"
date: 2012-02-25 17:30
comments: true
categories: 
---
In this discussion thread, Daniel Fischer marked the difference between Machine Word type and Fixed-Width Word type in haskell. I summarize them in a list, and attach to it with my own little experiment on the performance issue.

In haskell, the signed and unsigned integer type correspond to Int of Data.Int and Word of Data.Word respectively.
* The guaranteed range of Int is [-2²⁹ .. 2²⁹-1], and Word is the same size as Int.
* Normally the size of Int and Word are the word size of the underlying architecture.
* In general, the WordXX with XX less than the architecture machine word size, the machine word size is used for computation.
* WordXX with XX greater than machine word size, it could be a library implementation. Hence slower than using XX as machine word size.
* WordXX and Word with XX equals to machine word size, there is no difference.

Here comes a summation of 10⁹ 1 with different word types. The experiment is run on ThinkPad X61, Core 2 Duo, 32 bits, with Ubuntu GNU/Linux Natty.

```
-# LANGUAGE BangPatterns #-}
import Data.Word
import Data.List

sum_word :: Word -> Word
sum_word n = k n 0
    where k 0 s = s
          k !n !s = k (n-1) (s+1)

main = do
  print $ sum_word 1000000000
```

```
$ ghc -O2 -ddump-simpl Word.hs
$ time ./Word
1000000000

real 0m 6.094s
user 0m 6.088s
sys 0m 0.000s
```

```
{-# LANGUAGE BangPatterns #-}
import Data.Word
import Data.List

sum_word32 :: Word32 -> Word32
sum_word32 n = k n 0
    where k 0 s = s
          k !n !s = k (n-1) (s+1)

main = do
    print $ sum_word32 1000000000
```

```
$ ghc -O2 -ddump-simpl Word32.hs
$ time ./Word32
1000000000

real 0m 6.110s
user 0m 6.080s
sys 0m 0.012s
```

```
{-# LANGUAGE BangPatterns #-}
import Data.Word
import Data.List

sum_word64 :: Word64 -> Word64
sum_word64 n = k n 0
    where k 0 s = s
          k !n !s = k (n-1) (s+1)

main = do
    print $ sum_word64 1000000000
```

```
$ ghc -O2 -ddump-simpl Word64.hs
$ time ./Word64
1000000000

real 1m 52.981s
user 1m 52.743s
sys  0m 0.152s
```

Obviously, the Word64 version is much slower. If we vimdiff the core language of Word, Word16, Word32 versions, there is barely difference. The common main part is this:

```
Rec {
Main.$wk [Occ=LoopBreaker]
  :: GHC.Prim.Word# -> GHC.Prim.Word# -> GHC.Prim.Word#
[GblId, Arity=2, Caf=NoCafRefs, Str=DmdType LL]
Main.$wk =
  \ (ww_sXF :: GHC.Prim.Word#) (ww1_sXJ :: GHC.Prim.Word#) ->
    case GHC.Prim.eqWord# ww_sXF __word 0 of _ {
      GHC.Bool.False ->
        Main.$wk
          (GHC.Prim.minusWord# ww_sXF __word 1)
          (GHC.Prim.plusWord# ww1_sXJ __word 1);
      GHC.Bool.True -> ww1_sXJ
    }
end Rec }
```

We can see the unboxed GHC.Prim.Word# is used.

As to Word64 version, we can see __pkg_ccall everywhere in the generated core language.

```
Main.main4 :: GHC.Word.Word64
[GblId,
 Str=DmdType,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=0, Value=False,
         ConLike=False, Cheap=False, Expandable=False,
         Guidance=IF_ARGS [] 11 2}]
Main.main4 =
  case {__pkg_ccall ghc-prim hs_intToInt64 GHC.Prim.Int#
                                    -> GHC.Prim.State# GHC.Prim.RealWorld
                                    -> (# GHC.Prim.State# GHC.Prim.RealWorld, GHC.Prim.Int64# #)}_ayE
         1 GHC.Prim.realWorld#
  of _ { (# _, ds2_ayL #) ->
  case {__pkg_ccall ghc-prim hs_int64ToWord64 GHC.Prim.Int64#
                                       -> GHC.Prim.State# GHC.Prim.RealWorld
                                       -> (# GHC.Prim.State# GHC.Prim.RealWorld,
                                             GHC.Prim.Word64# #)}_ayJ
         ds2_ayL GHC.Prim.realWorld#
  of _ { (# _, ds4_ayQ #) ->
  GHC.Word.W64# ds4_ayQ
  }
}
```

Everything matches to what Daniel Fischer has described.
