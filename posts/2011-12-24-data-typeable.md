---
layout: post
title: "What are Data.Typeable and Data.Dynamic in Haskell"
date: 2011-12-24 17:30
comments: true
categories: 
---
While you are tracing serious haskell package, you probably would bump into something like this and wonder what it is.

```
foo :: Typeable a => a -> Maybe b
```

Then you look it up on hackage and see the following confusing paragraph.

The Typeable class reifies types to some extent by associating type representations to types. These type representations can be compared, and one can in turn define a type-safe cast operation. To this end, an unsafe cast is guarded by a test for type (representation) equivalence. The module Data.Dynamic uses Typeable for an implementation of dynamics. The module Data.Data uses Typeable and type-safe cast (but not dynamics) to support the “Scrap your boilerplate” style of generic programming.
You could also google to don’s writeup on stackoverflow giving the references. Don gave four of the references but I found two of them most related, they are:

Lammel, Ralf and Jones, Simon Peyton, “Scrap your boilerplate: a practical design pattern for generic programming, TLDI ’03: Proceedings of the 2003 ACM SIGPLAN International Workshop on Types in Languages Design and Implementation, 2003
Martín Abadi, Luca Cardelli, Benjamin Pierce and Gordon Plotkin, “Dynamic Typing in a Statically Typed Language”, ACM Transactions on Programming Languages and Systems (TOPLAS), 1991.
And here is my understanding after reading these two and doing a little experiment with GHC.

Typeable literally means able to ask its type (tag). It’s a typeclass defined in Data.Typeable with a function typeOf :: a -> TypeRep in it. Do you feel familiar with keyword typeOf? It reminds you either typeof in javascript, typeid in C++, or instanceof in java. Typeable mechanism is kind of like Reflection, using typeOf (1::Int) can tell you the type representation (or you can just think of it type) of (1::Int) There are numerous types in standard library defined as instances of Typeable, so I do a little experiment by printing their type represection.

```
import Data.Int
import Data.Word
import Data.Ord
import Data.Typeable
import Control.Monad.ST
import Control.Exception
import Foreign.C.Types

main = do
    putStrLn $ show $ typeOf (True::Bool)
    putStrLn $ show $ typeOf ('c'::Char)
    putStrLn $ show $ typeOf (1.0::Double)
    putStrLn $ show $ typeOf (1.0::Float)
    putStrLn $ show $ typeOf (1::Int)
    putStrLn $ show $ typeOf (1::Int8)
    putStrLn $ show $ typeOf (1::Int16)
    putStrLn $ show $ typeOf (1::Int32)
    putStrLn $ show $ typeOf (1::Int64)
    putStrLn $ show $ typeOf (1::Integer)
    putStrLn $ show $ typeOf (undefined::Ordering)
    putStrLn $ show $ typeOf (undefined::RealWorld)
    putStrLn $ show $ typeOf (undefined::Word)
    putStrLn $ show $ typeOf (undefined::Word8)
    putStrLn $ show $ typeOf (undefined::Word16)
    putStrLn $ show $ typeOf (undefined::Word32)
    putStrLn $ show $ typeOf (undefined::Word64)
    putStrLn $ show $ typeOf (undefined::())
    putStrLn $ show $ typeOf (undefined::TyCon)
    putStrLn $ show $ typeOf (undefined::TypeRep)
    putStrLn $ show $ typeOf (undefined::ArithException)
    putStrLn $ show $ typeOf (undefined::ErrorCall)
    putStrLn $ show $ typeOf (undefined::SomeException)
    putStrLn $ show $ typeOf (undefined::IOException)
    putStrLn $ show $ typeOf (undefined::CUIntMax)
    putStrLn $ show $ typeOf (undefined::CIntMax)
    putStrLn $ show $ typeOf (undefined::CUIntPtr)
    putStrLn $ show $ typeOf (undefined::CIntPtr)
Bool
Char
Double
Float
Int
Int8
Int16
Int32
Int64
Integer
Ordering
RealWorld
Word
Word8
Word16
Word32
Word64
()
TyCon
TypeRep
GHC.Exception.ArithException
GHC.Exception.ErrorCall
GHC.Exception.SomeException
GHC.IO.Exception.IOException
CUIntMax
CIntMax
CUIntPtr
CIntPtr
```

You can see that from ArithException, the output becomes GHC.Exception.ArithException, and ErrorCall becomes GHC.Exception.ErrorCall, not just boring identity from Int to Int, Char to Char. typeOf not just show it’s type but some type representation.

It’s first official definition was shown in “scrap your boilerplate.” Here is the working excerpt of a reference implementation from the paper.

```
import Unsafe.Coerce

class Typeable a where
    typeOf :: a -> TypeRep

data TypeRep = TR String [TypeRep] deriving (Eq, Show)

instance Typeable Int where
    typeOf x = TR "Prelude.Int" []

instance Typeable Bool where
    typeOf x = TR "Prelude.Bool" []

instance Typeable a => Typeable [a] where
    typeOf x = TR "Prelude.List" [typeOf (get x)]
        where
            get :: [a] -> a
            get = undefined

instance (Typeable a, Typeable b) => Typeable (a->b) where
    typeOf f = TR "Prelude.->" [typeOf (getArg f), typeOf (getRes f)]
        where getArg :: (a->b) -> a
              getArg = undefined
              getRes :: (a->b) -> b
              getRes = undefined

cast :: (Typeable a, Typeable b) => a -> Maybe b
cast x = r
    where
        r = if typeOf x == typeOf (get r)
                then Just (unsafeCoerce x)
                else Nothing
        get :: Maybe a -> a
        get x = undefined

main = do
    putStrLn $ show $ typeOf (1::Int)
```

Although the intention to define Typeable typeclass in “scrap your boilerplate” is for generic programming, it is found useful in defining dynamic type, and that is what Data.Dynamic used for. As the previous quote says:

The module Data.Dynamic uses Typeable for an implementation of dynamics.
Then why do we need dynamic type in a static type language? An excerpt from “Dynamic Typing in a Statically Typed Language” tell us. > There is often the need to deal with data whose type cannot be > determined at compile time. For example, full static typechecking > of programs that exchange data with other programs or access persistent > data is in general not possible. A certain amount of dynamic checking must > be performed in order to preserve type safety.

A solution to this is to define a static Dynamic type, which internally could be considered as a tuple of (binary representation, type tag). When the program communicates with external resources, all of the type is converted to Dynamic. You could achieve runtime type safety by inspecting its type tag (with typeOf function)

An example of this is Heterogeneous List

```
import Data.Dynamic
import Data.Maybe

hlist :: [Dynamic]
hlist = [toDyn "string",
         toDyn (7::Int),
         toDyn (pi :: Double),
         toDyn 'x',
         toDyn ((), Just "foo")]

dyn :: Dynamic
dyn = hlist !! 1

v :: Int
v = case fromDynamic dyn of
         Nothing -> error "Type mismatch"
         Just x -> x

main = do putStrLn $ show v
```

Postscript: Haskell 98 limits the typeclass that is auto derivable. To switch on auto derivation of Typeable, please check auto deriving
