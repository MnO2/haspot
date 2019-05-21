---
layout: post
title: "Digging Deeper at Haskell Instance Definition"
date: 2011-12-07 17:30
comments: true
categories: 
---
If you try to compiler the following clip of code with ghc

```
class C a where
    toString :: a -> String

instance C String where
    toString = id

main = do
    putStrLn $ toString "Hello World"
```

The compiler will pop out the error message.

```
test01.hs:4:10:
    Illegal instance declaration for `C String'
    (All instance types must be of the form (T t1 ... tn)
     where T is not a synonym.
     Use -XTypeSynonymInstances if you want to disable this.)
In the instance declaration for `C String'
```

What does it mean? Let’s try another run with much simpler type.

```
class C a where
    toString :: a -> String

instance C Int where
    toString = show

instance C Double where
    toString = show

main = do
    putStrLn $ toString (123::Int)
    putStrLn $ toString (123.123::Double)
```

What? it works very well.

Give another try. We replace String to [Char].

```
class C a where
    toString :: a -> String

instance C [Char] where
    toString = id

main = do
    putStrLn $ toString "Hello World"
```

But it still doesn’t work.

```
test03.hs:4:10:
    Illegal instance declaration for `C [Char]'
    (All instance types must be of the form (T a1 ... an)
     where a1 ... an are *distinct type variables*,
     and each type variable appears at most once in the instance head.
     Use -XFlexibleInstances if you want to disable this.)

In the instance declaration for `C [Char]'
```

Suprisingly, this does work.

```
class C a where
    toString :: a -> String

instance Show a => C [a] where
    toString = concat . (map show)

main = do
    putStrLn $ toString ([1,2,3,4]::[Int])
```

So the problem is not about list constructor []?

Let’s go to a simple cure directly. That is to add {-# LANGUAGE FlexibleInstances #-}, i.e. use FlexibleInstances GHC extension.

```
{-# LANGUAGE FlexibleInstances #-}
class C a where
    toString :: a -> String

instance C Int where
    toString = show

instance C Double where
    toString = show

instance C [Char] where
    toString = id

instance C [Int] where
    toString = concat . (map show)

main = do
    putStrLn $ toString (123::Int)
    putStrLn $ toString "Hello World"
    putStrLn $ toString ([1,2,3,4,5]::[Int])
```

That works for C [Char], but still doesn’t work for C String

```
{-# LANGUAGE FlexibleInstances #-}
class C a where
    toString :: a -> String

instance C String where
    toString = id

main = do
    putStrLn $ toString "Hello World"
test06.hs:5:10:
    Illegal instance declaration for `C String'
    (All instance types must be of the form (T t1 ... tn)
     where T is not a synonym.
     Use -XTypeSynonymInstances if you want to disable this.)
```

In the instance declaration for `C String'
What’s going on? Let’s check the Haskell 98 Report Sec. 4.3.2

The type (T u1 … uk) must take the form of a type constructor T applied to simple type variables u1, … uk; furthermore, T must not be a type synonym, and the ui must all be distinct.
This prohibits instance declarations such as:

```
instance C (a,a) where …
instance C (Int,a) where …
instance C [[a]] where …
```

So according to the report, C [a] is legal, since [a] = [] a where T = [] and u1 = a. But String = [Char] = [] Char, where T = [] and Char is not a type variable. As to why the specific type is invalid. Suppose we have

```
instance C [Char] where ... instance C [Int] where ...
```

A given constraint of C [a] for some type variable a, we cannot decide which instance declaration to use unless we know about a. And in Haskell 98 the comittee consider to avoid that. For detailed explanation, go ahead to read SPJ’s paper Sec.4.5

The result is as expected if we try Type synonym, as the standard said the Type synonym is forbidden.

```
type Identifier = String

class C a where
    toString :: a -> String

instance C Identifier where
    toString = show

main = do
    putStrLn $ toString ("Hello World"::Identifier)
```

Flexible Instances GHC extension is to give us more flexible instance defition form, but I didn’t check how the issue above was resolved.

Excerpt from Flexible Instances GHC extension > GHC 6.5 > … > > This rule allows instances accepted by the previous rule and more, including > > instance C a > > instance Show (s a) => Show (Sized s a) > > instance (C1 a, C2 b) => C a b > > instance C1 Int a => C2 Bool [a] > > instance C1 Int a => C2 [a] b > > instance C a a => C [a] [a]

To resolve the issue and at the same time conforming Haskell 98, This link listed two methods.

One is to use newtype

```
class C a where
    toString :: a -> String

newtype Identifier = Identifier String deriving (Show)

instance C Identifier where
    toString = show

main = do
    putStrLn $ toString (Identifier "Hello")
```

Another way is to define another listToString

```
class C a where
    toString :: a -> String
    listToString :: [a] -> String

instance C Char where
    toString = show
    listToString = id

instance C a => C [a] where
    toString = listToString

main = do
    putStrLn $ toString ("Hello"::[Char])
```
