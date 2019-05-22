---
layout: post
title: "Haskell Library Survey: Blaze-Builder"
date: 2012-03-17 17:30
comments: true
categories: 
---
Haskell is notorious for plethoric libraries on Hackage. For a beginner, it’s easy to lose in the forest of Hackage. With the establishment of Haskell Platform and the advice from experts ( link1, link2 ). the situation is getting better. But for a community, more tutorials would be more plausible. Therefore I decide to write down my library survey.

### What is this library for?
If you remembered it well in Learn you a Haskell for Great Good, it mentions that the List append operation (++) takes linear time. It’s implementation in Haskell 98 Report looks like this:

```
infixr 5  ++
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
```

It’s obvious you have to traverse through all of the elements in the first list to get the operation done, hence linear time. And fortunately, ++ is right associative, which means when you are writing snippets like this, assuming as, bs, cs are all lists.

It essentially means

```
(as ++ (bs ++ cs))
```

Based on the previous reasoning, it won’t cost us too much, merely the length of as plus the length of bs.

But there IS some common cases that gets us in trouble. Let’s take a look at this example from here:

appendToContent str page = page {pageContent = newContent}
where newContent = (pageContent page) ++ str
It is a naiive implementation usually occurs in Logger or the streaming output of a HTTP server. The problem of this pattern is it forces the concatenation to be left associative. Using the previous as ++ bs ++ cs as our example, it will become:

```
((as ++ bs) ++ cs)
```

It is unacceptable because the left associative ++ costs us quadratic time in the size of resulting list. To see that, here is an excerpt from ezyang’s post

For clarity, we rewrite the definition from Haskell 98 to a ++ b = foldr (:) b a, the time complexity is the same.

```
(as ++ bs) ++ cs
= foldr (:) cs (as ++ bs)
= foldr (:) cs (foldr (:) bs as)
= foldr (:) cs (foldr (:) bs (a:as'))
= foldr (:) cs (a : foldr (:) b as')
= a : foldr (:) cs (foldr (:) bs as')
```

“This is normal, each string append operation costs you linear time in most programming languages after all.” you said. “Joel Spolsky even make this problem famously known as Schlemiel the Painter’s algorithm.”

But sometimes we DO care about performance. Logger and streaming output of a HTTP server previously mentioned are two of them. We really need efficient list append operation

There are a few reasonable tactics according to Norman Ramsey

Standard list with cons.
Difference list (John Hughes list) with constant-time append.
Algebraic data type supporting constant-time append: data Alist a = ANil | ASingle a | AAppend (Alist a) (Alist a).
List of lists with final concat.
It depends on the problem you are working to choose the most appropriate one. Here we focus on the second one, Difference List, because it is the one used in the blaze-builder. It’s originally designed for the streaming output of html. Therefore the append is truly the issue we care about.

### Difference List
The difference list is a data strucutre empowers us constant-time append. As the definition given by Haskell Wiki:

It is a function f, which when given a list x, returns the list that f represents, prepended to x.
Hard to imagine? Let’s take a look at a concrete example defined in Haskell: the ShowS type: The difference list specifically for List of Char.

Its type in Haskell 98 Report is:

```
type ShowS = String -> String
```

Judging from its type, it basically matches the description of a difference list. It is a function accepting a String, and supposedly output a String prepending something to x.

Here is a toy example, using snoc (the inverse of cons) to append Char one by one to the end of difference list.

```
toString :: ShowS -> String
toString = ($ [])
snoc ∷ ShowS → Char → ShowS
snoc f a = f ∘ (a:)
emptyH ∷ ShowS
emptyH = id
body ∷ String → ShowS → ShowS
body (x:xs) acc = body xs (acc `snoc` x)
body _ acc = acc
run ∷ String → String
run xs = toString (body xs emptyH)
main = putStrLn $ run "1234"
```

We heavily rely on the function composition of (a:). With composition we can achieve the constant-time append, and stil preserve some properties of a list. To convert a difference list to a normal list of Char, apply the function to an empty list []. It is what toString is doing.

The introduction of a function enables us to evaluate left-associated construction in right-associated fashion.

```
(as . (bs . cs)) 
= as . bs .cs
= (as . (bs . cs))
```

With ShowS, we can rewrite the previous example with showString, which is defined as showString s = (s++)`` haskell appendToContent str page = page {pageContent = newContent} where newContent = (pageContent page) . showString str ```

Another excerpt from ezyang’s post proves the work of a difference list. (called Hughes List here)

```
type Hughes a = [a] -> [a]
listrep :: Hughes a -> [a]
listrep = (\l -> l [])
append :: Hughes a -> Hughes a -> Hughes a
append a b = (\z -> a (b z))
listrep (append (append a b) c)
= (\l -> l []) (append (append a b) c)
= (append (append a b) c) []
= (\z -> (append a b) (c z)) []
= (append a b) (c [])
= (\z -> a (b z)) (c [])
= a (b (c []))
```

### Blaze-Builder
Difference list is not limited to list of Char. Elaborating on ShowS is merely for explanation. It can be generalized to list of Chunks, or ByteString for performant output.

Data.Binary.Builder was frontier to use this pattern. The internal working of provided Builder type is essentially a difference list. The purpose of binary package is to encode the Haskell value into binary format and serialize them to external devices, network devices. And blaze-builder, as in Simon’s and Jaspervdj’s introduction (The latter one is a little bit outdated), it is a drop-in replacement for Data.Binary.Builder, with speed and expressiveness enhancement.

A toy example from the documentation:

```
import Data.Monoid
import qualified Data.ByteString.Lazy as L
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
strings :: [String]
strings = replicate 10000 "Hello World!"
concatenation :: Builder
concatenation = mconcat $ map fromString strings
result :: L.ByteString
result = toLazyByteString concatenation
main = L.putStrLn result
```

This example serializes 10000 instances of “Hello World” to binary representation, which coincidently to be ASCII encoding of bytes. We use provided fromString to serialize a single String to Builder. And the Builder provides a Monoid interface, which means we have mconcat to efficiently concat them, as we do for the difference list.

After that, we use toLazyByteString to transform it from Builder to ByteString, just like we call toString in the ShowS example.

With ByteString we have putStrLn function to output the binary representation to external devices.

### Postscript
According to this slide by Simon Meiser. It seems that he is working on bytestring package to incorporate similiar techniques used on blaze-builder. Perhaps after a while the blaze-builder could be replaced by binary. But at this time, a lot of packages are still using blaze-builder. It is still worth it to have a basic understanding of what this library is doing.
