---
layout: post
title: "What are Data.Typeable and Data.Dynamic in Haskell"
date: 2011-12-24 07:13
comments: true
categories: haskell 
---

While you are tracing serious haskell package, you probably would bump into something like this and wonder what it is.

```
foo :: Typeable a => a -> Maybe b
```

Then you look it up on [hackage](http://haskell.org/ghc/docs/latest/html/libraries/base-4.4.1.0/Data-Typeable.html) and see the following confusing paragraph.

> The Typeable class reifies types to some extent by associating type representations to types. 
> These type representations can be compared, and one can in turn define a type-safe cast operation. 
> To this end, an unsafe cast is guarded by a test for type (representation) equivalence. 
> The module Data.Dynamic uses Typeable for an implementation of dynamics. 
> The module Data.Data uses Typeable and type-safe cast (but not dynamics) 
> to support the "Scrap your boilerplate" style of generic programming.

You could also google to [don's writeup](http://stackoverflow.com/questions/6600380/what-is-haskells-data-typeable) on stackoverflow giving the references.
Don gave four of the references but I found two of them most related, they are:

* Lammel, Ralf and Jones, Simon Peyton, "Scrap your boilerplate: a practical design pattern for generic programming, TLDI '03: Proceedings of the 2003 ACM SIGPLAN International Workshop on Types in Languages Design and Implementation, 2003
* Martín Abadi, Luca Cardelli, Benjamin Pierce and Gordon Plotkin, "Dynamic Typing in a Statically Typed Language", ACM Transactions on Programming Languages and Systems (TOPLAS), 1991.

And here is my understanding after reading these two and doing a little experiment with GHC.

`Typeable` literally means `able to ask its type (tag)`.
It's a typeclass defined in `Data.Typeable` with a function `typeOf :: a -> TypeRep` in it.
Do you feel familiar with keyword `typeOf`? 
It reminds you either `typeof` in javascript, `typeid` in C++, or `instanceof` in java.
Typeable mechanism is kind of like Reflection, using `typeOf (1::Int)` can tell you the type representation (or you can just think of it type) of `(1::Int)`
There are numerous types in standard library defined as instances of `Typeable`, so I do a little experiment by printing their type represection.

{% gist 1517679 data_typeable.hs %}

```
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

You can see that from `ArithException`, the output becomes `GHC.Exception.ArithException`, and `ErrorCall` becomes `GHC.Exception.ErrorCall`,
not just boring identity from `Int` to `Int`, `Char` to `Char`.
`typeOf` not just show it's type but some *type representation*.

It's first official definition was shown in "scrap your boilerplate."
Here is the working excerpt of a reference implementation from the paper.

{% gist 1517679 scrap_your_boilerplate.hs %}

Although the intention to define `Typeable` typeclass in "scrap your boilerplate" is for generic programming,
it is found useful in defining dynamic type, and that is what `Data.Dynamic` used for.
As the previous quote says:

> The module Data.Dynamic uses Typeable for an implementation of dynamics.

Then why do we need dynamic type in a static type language?
An excerpt from "Dynamic Typing in a Statically Typed Language" tell us.
> There is often the need to deal with data whose type cannot be
> determined at compile time. For example, full static typechecking
> of programs that exchange data with other programs or access persistent
> data is in general not possible. A certain amount of dynamic checking must
> be performed in order to preserve type safety.

A solution to this is to define a static `Dynamic` type, 
which internally could be considered as a tuple of `(binary representation, type tag)`.
When the program communicates with external resources,
all of the type is converted to `Dynamic`.
You could achieve runtime type safety by inspecting its type tag (with `typeOf` function)

An example of this is [Heterogeneous List](http://www.haskell.org/haskellwiki/Heterogenous_collections)

{% gist 1517679 data_dynamic.hs %}

Postscript:
Haskell 98 limits the typeclass that is auto derivable.
To switch on auto derivation of `Typeable`, please check [auto deriving](http://www.haskell.org/ghc/docs/latest/html/users_guide/deriving.html)

