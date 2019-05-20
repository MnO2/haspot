---
layout: post
title: "Floated Out and Eta Expansion"
date: 2012-01-17 17:30
comments: true
categories: 
---
This write-up was inspired by a question on stackoverflow. The answers explain almost everything, but I still want to dig deeper from GHC implementation. That is to say, I am viewing this problem with core langauge generated with option -ddump-simpl when you compile haskell programs.
Let's start off with an odd number filter.

```
f1 = ((filter odd [1..]) !!)

f2 x = ((filter odd [1..]) !! x)

main = do
    print $ f1 10000
    print $ f1 10000
    print $ f2 10000
    print $ f2 10000
Without optimization, we get a simple core generated. (I've removed unrelated I/O parts and cleaned it up.)
f2_rbu
  :: forall a_ase. GHC.Real.Integral a_ase => GHC.Types.Int -> a_ase
[GblId, Arity=2]
f2_rbu =
  \ (@ a_ase)
    ($dIntegral_asf :: GHC.Real.Integral a_ase)
    (eta_B1 :: GHC.Types.Int) ->
    GHC.List.!!
      @ a_ase
      (GHC.List.filter
         @ a_ase
         (GHC.Real.odd @ a_ase $dIntegral_asf)
         (GHC.Enum.enumFrom
            @ a_ase
            (GHC.Real.$p2Integral @ a_ase $dIntegral_asf)
            (GHC.Num.fromInteger
               @ a_ase
               (GHC.Real.$p1Real
                  @ a_ase (GHC.Real.$p1Integral @ a_ase $dIntegral_asf))
               (GHC.Integer.smallInteger 1))))
      eta_B1

f1_rbs :: GHC.Types.Int -> GHC.Integer.Type.Integer
[GblId]
f1_rbs =
  GHC.List.!!
    @ GHC.Integer.Type.Integer
    (GHC.List.filter
       @ GHC.Integer.Type.Integer
       (GHC.Real.odd
          @ GHC.Integer.Type.Integer GHC.Real.$fIntegralInteger)
       (GHC.Enum.enumFrom
          @ GHC.Integer.Type.Integer
          GHC.Num.$fEnumInteger
          (GHC.Integer.smallInteger 1)))
```

f1_rbs corresponds to f1 and f2_rbu corresponds to f2 in the source code. You can see that f2_rbu has the form of \eta_B1 -> (!!) filter… eta_B1, but f1_rbs without a surrounding lambda instead (!!) filter…. Clearly, f2_rbu is the eta expansion of f1_rbs. We also know that the inside of a lamda is evaluated each time we call it. Therefore, calling f2_rbu multiple times would results in reconstruction of list. In f1_rbs case, the list is shared among different callings.
Next we examine the optimized case with -O2 switched on.

```
Main.main6 =
  \ (x_aAr :: GHC.Integer.Type.Integer)
    (r_aAs :: [GHC.Integer.Type.Integer]) ->
    case GHC.Integer.remInteger x_aAr GHC.Real.even2 of _ {
      GHC.Integer.Type.S# i_aCs ->
        case i_aCs of _ {
          __DEFAULT -> GHC.Types.: @ GHC.Integer.Type.Integer x_aAr r_aAs;
          0 -> r_aAs
        };
      GHC.Integer.Type.J# s_aCx d_aCy ->
        case {__pkg_ccall_GC integer-gmp integer_cmm_cmpIntegerIntzh GHC.Prim.Int#
                                                        -> GHC.Prim.ByteArray#
                                                        -> GHC.Prim.Int#
                                                        -> GHC.Prim.Int#}_aCw
               s_aCx d_aCy 0
        of _ {
          __DEFAULT -> GHC.Types.: @ GHC.Integer.Type.Integer x_aAr r_aAs;
          0 -> r_aAs
        }
    }

Main.main5 = GHC.Integer.Type.S# 1

Main.main4 =
  GHC.Num.enumDeltaIntegerFB
    @ [GHC.Integer.Type.Integer] Main.main6 Main.main5 Main.main5

Main.main3 = GHC.List.!!_sub @ GHC.Integer.Type.Integer Main.main4 10000

Main.main2 = GHC.Num.$w$cshowsPrec 0 Main.main3 (GHC.Types.[] @ GHC.Types.Char)




Main.main10 =
  \ (x_aAr :: GHC.Integer.Type.Integer)
    (r_aAs :: [GHC.Integer.Type.Integer]) ->
    case GHC.Integer.remInteger x_aAr GHC.Real.even2 of _ {
      GHC.Integer.Type.S# i_aCs ->
        case i_aCs of _ {
          __DEFAULT -> GHC.Types.: @ GHC.Integer.Type.Integer x_aAr r_aAs;
          0 -> r_aAs
        };
      GHC.Integer.Type.J# s_aCx d_aCy ->
        case {__pkg_ccall_GC integer-gmp integer_cmm_cmpIntegerIntzh GHC.Prim.Int#
                                                        -> GHC.Prim.ByteArray#
                                                        -> GHC.Prim.Int#
                                                        -> GHC.Prim.Int#}_aCw
               s_aCx d_aCy 0
        of _ {
          __DEFAULT -> GHC.Types.: @ GHC.Integer.Type.Integer x_aAr r_aAs;
          0 -> r_aAs
        }
    }

Main.main9 =
  GHC.Num.enumDeltaIntegerFB
    @ [GHC.Integer.Type.Integer] Main.main10 Main.main5 Main.main5

Main.main8 = GHC.List.!!_sub @ GHC.Integer.Type.Integer Main.main9 10000

Main.main7 = GHC.Num.$w$cshowsPrec 0 Main.main8 (GHC.Types.[] @ GHC.Types.Char)

Main.main1 =
  \ (s_XYA :: GHC.Prim.State# GHC.Prim.RealWorld) ->
    case GHC.IO.Handle.Text.hPutStr2
           GHC.IO.Handle.FD.stdout Main.main7 GHC.Bool.True s_XYA
    of _ { (# new_s_aXy, _ #) ->
    case GHC.IO.Handle.Text.hPutStr2
           GHC.IO.Handle.FD.stdout Main.main7 GHC.Bool.True new_s_aXy
    of _ { (# new_s1_XYJ, _ #) ->
    case GHC.IO.Handle.Text.hPutStr2
           GHC.IO.Handle.FD.stdout Main.main2 GHC.Bool.True new_s1_XYJ
    of _ { (# new_s2_XYF, _ #) ->
    GHC.IO.Handle.Text.hPutStr2
      GHC.IO.Handle.FD.stdout Main.main2 GHC.Bool.True new_s2_XYF
    }
    }
    }
```

The optimized code is a little confusing. The optimized f1 part is definitions from Main.main2 to Main.main6, and optimized f2 part is from Main.main7 to Main.main10. Without too much insight you can observe that the content of Main.main6 and Main.main10 are almost the same. The surrounding lambda of f2 is optimized away.
Now go back to the fibonacci program in the question.

```
fib = let fib' 0 = 0
          fib' 1 = 1
          fib' n = fib (n-1) + fib (n-2)
      in (map fib' [0 ..] !!)

fib2 x = let fib' 0 = 0
             fib' 1 = 1
             fib' n = fib2 (n-1) + fib2 (n-2)
         in map fib' [0 ..] !! x

main = do
    print $ fib 100
    print $ fib2 100
```

And this is unoptimized version.

```
(letrec {
  fib_aoC [Occ=LoopBreaker]
    :: GHC.Types.Int -> GHC.Integer.Type.Integer
  [LclId]
  fib_aoC =
    GHC.List.!!
      @ GHC.Integer.Type.Integer
      (GHC.Base.map
         @ GHC.Types.Int
         @ GHC.Integer.Type.Integer
         (\ (ds_ds5 :: GHC.Types.Int) ->
            case ds_ds5 of wild_B1 { GHC.Types.I# ds1_ds6 ->
            case ds1_ds6 of _ {
              __DEFAULT ->
                GHC.Num.+
                  @ GHC.Integer.Type.Integer
                  GHC.Num.$fNumInteger
                  (fib_aoC
                     (GHC.Num.-
                        @ GHC.Types.Int GHC.Num.$fNumInt wild_B1 (GHC.Types.I# 1)))
                  (fib_aoC
                     (GHC.Num.-
                        @ GHC.Types.Int GHC.Num.$fNumInt wild_B1 (GHC.Types.I# 2)));
              0 -> GHC.Integer.smallInteger 0;
              1 -> GHC.Integer.smallInteger 1
            }
            })
         (GHC.Enum.enumFrom
            @ GHC.Types.Int GHC.Enum.$fEnumInt (GHC.Types.I# 0))); } in
fib_aoC (GHC.Types.I# 100))


(letrec {
  fib2_acd [Occ=LoopBreaker]
    :: GHC.Types.Int -> GHC.Integer.Type.Integer
  [LclId, Arity=1]
  fib2_acd =
    \ (x_abA :: GHC.Types.Int) ->
      GHC.List.!!
        @ GHC.Integer.Type.Integer
        (GHC.Base.map
           @ GHC.Types.Int
           @ GHC.Integer.Type.Integer
           (\ (ds_ds0 :: GHC.Types.Int) ->
              case ds_ds0 of wild_B1 { GHC.Types.I# ds1_ds1 ->
              case ds1_ds1 of _ {
                __DEFAULT ->
                  GHC.Num.+
                    @ GHC.Integer.Type.Integer
                    GHC.Num.$fNumInteger
                    (fib2_acd
                       (GHC.Num.-
                          @ GHC.Types.Int GHC.Num.$fNumInt wild_B1 (GHC.Types.I# 1)))
                    (fib2_acd
                       (GHC.Num.-
                          @ GHC.Types.Int GHC.Num.$fNumInt wild_B1 (GHC.Types.I# 2)));
                0 ->
                  GHC.Num.fromInteger
                    @ GHC.Integer.Type.Integer
                    GHC.Num.$fNumInteger
                    (GHC.Integer.smallInteger 0);
                1 ->
                  GHC.Num.fromInteger
                    @ GHC.Integer.Type.Integer
                    GHC.Num.$fNumInteger
                    (GHC.Integer.smallInteger 1)
              }
              })
           (GHC.Enum.enumFrom
              @ GHC.Types.Int GHC.Enum.$fEnumInt (GHC.Types.I# 0)))
        x_abA; } in
fib2_acd (GHC.Types.I# 100))
```

You can see the pattern that one has lambda outside and another doesn't. fib_aoC is (!!) (map (\ds_ds5 -> …) enumFrom…) and fib2_acd is \x_abA -> (!!) (map (\ds_ds0 -> …) enumFrom…) x_abA).
As for -O2 version. We can view the code in two groups:

* Main.main7, Main.main_sgo, go_rUQ, Main.amin6, Main.main5, which corresponds to fib.
* sgo_rUS, go1_rUW, a_rUV, Main.wfib2, Main.main3, which corresponds to fib2

```
Main.main7 = GHC.Integer.Type.S# 0

Main.main_$sgo = GHC.Types.: @ GHC.Integer.Type.Integer Main.main7 Main.main6

go_rUQ =
  \ (x_avk :: GHC.Prim.Int#) ->
    GHC.Types.:
      @ GHC.Integer.Type.Integer
      (case x_avk of ds_Xsb {
         __DEFAULT ->
           let {
             n0_asK [Dmd=Just L] :: GHC.Prim.Int#
             n0_asK = GHC.Prim.-# ds_Xsb 1 } in
           case GHC.Prim.<# n0_asK 0 of _ {
             GHC.Bool.False ->
               let {
                 n1_XtE [Dmd=Just L] :: GHC.Prim.Int#
                 n1_XtE = GHC.Prim.-# ds_Xsb 2 } in
               case GHC.Prim.<# n1_XtE 0 of _ {
                 GHC.Bool.False ->
                   GHC.Integer.plusInteger
                     (GHC.List.!!_sub @ GHC.Integer.Type.Integer Main.main_$sgo n0_asK)
                     (GHC.List.!!_sub @ GHC.Integer.Type.Integer Main.main_$sgo n1_XtE);
                 GHC.Bool.True ->
                   GHC.List.!!1
                   `cast` (CoUnsafe (forall a_asR. a_asR) GHC.Integer.Type.Integer
                           :: (forall a_asR. a_asR) ~ GHC.Integer.Type.Integer)
               };
             GHC.Bool.True ->
               GHC.List.!!1
               `cast` (CoUnsafe (forall a_asR. a_asR) GHC.Integer.Type.Integer
                       :: (forall a_asR. a_asR) ~ GHC.Integer.Type.Integer)
           };
         0 -> Main.main7;
         1 -> lvl_rUL
       })
      (case x_avk of wild_B1 {
         __DEFAULT -> go_rUQ (GHC.Prim.+# wild_B1 1);
         2147483647 -> GHC.Types.[] @ GHC.Integer.Type.Integer
       })

Main.main6 = go_rUQ 1

Main.main5 = GHC.List.!!_sub @ GHC.Integer.Type.Integer Main.main_$sgo 100

$sgo_rUS = GHC.Types.: @ GHC.Integer.Type.Integer Main.main7 a_rUV

go1_rUW =
  \ (x_avk :: GHC.Prim.Int#) ->
    GHC.Types.:
      @ GHC.Integer.Type.Integer
      (case x_avk of ds_Xs9 {
         __DEFAULT ->
           GHC.Integer.plusInteger
             (Main.$wfib2 (GHC.Prim.-# ds_Xs9 1))
             (Main.$wfib2 (GHC.Prim.-# ds_Xs9 2));
         0 -> Main.main7;
         1 -> lvl_rUL
       })
      (case x_avk of wild_B1 {
         __DEFAULT -> go1_rUW (GHC.Prim.+# wild_B1 1);
         2147483647 -> GHC.Types.[] @ GHC.Integer.Type.Integer
       })

a_rUV = go1_rUW 1

Main.$wfib2 =
  \ (ww_sU0 :: GHC.Prim.Int#) ->
    case GHC.Prim.<# ww_sU0 0 of _ {
      GHC.Bool.False ->
        GHC.List.!!_sub @ GHC.Integer.Type.Integer $sgo_rUS ww_sU0;
      GHC.Bool.True -> GHC.List.!!1 @ GHC.Integer.Type.Integer
    }
end Rec }

Main.main3 = Main.$wfib2 100

Main.main2 = GHC.Num.$w$cshowsPrec 0 Main.main3 (GHC.Types.[] @ GHC.Types.Char)
```

Both definitions are dissected into the form sgo (indirectly) calls go_XXX, and inside go_XXX it calls back sgo. The lambda is optimized away, hence the performance is improved.
The experiment setting is GHC 7.0.2 on ArchLinux, 32 bits. Here I merely try to view the problem from the core language, and I don't intend to try the affect of type hint as mentioned in the original thread.
