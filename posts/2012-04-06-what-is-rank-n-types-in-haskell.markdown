---
layout: post
title: "What is Rank-N Types in Haskell"
date: 2012-04-06 21:41
comments: true
categories: haskell 
---

For a beginner of haskell coming from popular imperative languages with "Generic" or "Template" construct in them, 
like C++, Java, it is not too hard to accept the parametric polymorphism at first glance.
The type signature of ``id :: a -> a`` can quickly remind him of C++ template.

``` cpp
#include <iostream>
#include <string>

using namespace std;

template <typename T>
T id(const T &x)
{
        return x; 
}

int main()
{
        cout << id(1) << endl;
        cout << id(string("foo")) << endl;
        return 0;
}
```

However, the thing would become complicated when he bumped to the type signature of ``runST``

``` haskell
runST :: (forall s. ST s a) -> a
```

A few questions pop out. What the hell is that ``forall``, and what are ``Rank2Types`` and ``RankNTypes`` documented in the GHC manual?

## Higher Rank Polymorphism

The parametric polymorphism you usually see is actually Rank-1 Polymorphism.
There is actually Rank-2 polymorphism ... to Rank-N polymorphism.
In not quite exact language, a Rank-1 polymorphic function is that it doesn't depend on its input/output types. 
The type become parameters, and when you call the function, the types are "instantiated."
And Rank-2 polymorphic function is a function gets called with its parameters parameterized, or to be Rank-1 polymorphic.
So on, Rank-N polymorphic function is a function gets called with its parameters to be Rank-(N-1) polymorphic.

Let's look at a Rank-2 example.
Suppose we have a list of ``Int`` and a list of ``Char``, we want to write a function to check if the results of applying a function to both lists are equal.
Use case would look like this.

``` haskell
check f l1 l2 = (f l1) == (f l2)

main = do
    print $ check length [1,2,3,4] ['a','b','c','d']
    print $ check ((+2) . length) [1,2] ['a']
```

We can see that we want parameter ``f`` of ``check`` to be a Rank-1 polymorphic function,
so that we can apply it to both ``[Int]`` and ``[Char]``.
In such case, what would be the type signature of ``check``?
Let's look at how we can declare it in haskell.

## forall
Parametric polymorphism is implicit in haskell. When you write ``a -> a``, actually it is ``forall a. a -> a``.
``forall`` is the keyword to explicitly declare what is your type parameter, or type variable.
A few examples from standard lib can be explicitly written like this.

``` haskell
length :: forall a. [a] -> Int
fst :: forall a b. (a,b) -> a
map :: forall a b. (a -> b) -> [a] -> [b]
```
Writing out explicitly, we declare the type signature of ``check`` as ``check :: (forall a. [a] -> Int) -> forall b. [b] -> forall c. [c] -> Bool``.
Notice that the effective range of ``forall`` greedily extends to the most right end, so the type signature above is equivalent to ``check :: (forall a. a -> [Int]) -> (forall b. [b] -> (forall c. [c] -> Bool))``.
But so many ``forall`` in the type signature, you would ask where should I put the ``forall`` to express a Rank-2 polymorphic function.
Just as this [link](http://thread.gmane.org/gmane.comp.lang.haskell.cafe/40508/focus=40610) said,
you can consider ``forall`` as a type level lambda ``/\``.
This lambda ``/\`` has some operational similarity with ``\``, instead of declare a variable binding, it binds type variables.
A parallel in C++ would be binding ``T`` of ``template <typename T>`` to the corresponding template.

With this in mind, basically we can write out the type variables and normal variables at the same time in ``/\`` and ``\``.
It would look like ``check = \f -> /\b -> \l1 -> /\c -> \l2 -> Bool``, where ``f`` is a function which is ``/\a -> \l -> Int``.

Here is a working example:
``` haskell
{-# LANGUAGE RankNTypes #-}
module Main(main) where

import Prelude hiding (length)

length :: forall a. [a] -> Int 
length (x:xs) = 1 + (length xs)
length [] = 0

check :: (forall a. [a] -> Int) -> forall b. [b] -> forall c. [c] -> Bool
check f l1 l2 = (f l1) == (f l2)

main = do
    print $ check length [1] ['a','b']
```

You can play with Rank-N Type with N greater than 2. The spirit is the same.
This is an example from [here](http://blog.solaris.bytelabs.org/articles/2008/08/10/playing-with-rank-n-types).

``` haskell
{-# LANGUAGE RankNTypes #-}
module Main (main) where

g1 :: a -> a 
g1 x = x

g2 :: (forall a. a -> a) -> (Bool, Char)
g2 f = (f True, f 'a')

g3 :: ((forall a. a -> a) -> (Bool, Char)) -> (Char, Bool)
g3 f = (\x -> (snd x, fst x)) (f g1)

g4 :: (((forall a. a-> a) -> (Bool, Char)) -> (Char, Bool)) -> (Bool, Char)
g4 f = (\x -> (snd x, fst x)) (f g2)

main = do
    putStrLn "Rank-2:"
    putStrLn . show . fst $ (g2 g1)
    putStrLn . show . snd $ (g2 g1)
    putStrLn "Rank-3:"
    putStrLn . show . fst . g3 $ g2
    putStrLn . show . snd . g3 $ g2
    putStrLn "Rank-4:"
    putStrLn . show . fst . g4 $ g3
    putStrLn . show . snd . g4 $ g3
```

## Possible Confusion
Due to the usage of ``forall`` keyword, it is very easy to confuse ``RankNTypes``, Existential Types and Scoped Type Variables.
You can go to check out [an excellent discussion on stackoverflow](http://stackoverflow.com/questions/3071136/what-does-the-forall-keyword-in-haskell-ghc-do).
Also don't miss out [John Lato's](http://johnlato.blogspot.com/2012/03/existential-quantification-pt-1.html) and [ezyang's](http://blog.ezyang.com/2010/10/existential-type-curry/) explanations on Existenatial Types.
