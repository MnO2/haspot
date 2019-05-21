---
layout: post
title: "Day of week in Japanese"
date: 2015-05-31 17:30
comments: true
categories: 
---
Solving online judge’s porblems, you would definitely keep dealing with the pattern of breaking a infinite loop on reading a specific input. For example, a variable “n” signifying the size of the input, but when it is specified as “0”, it represents the end of the input. Haskell’s IO Monad, however, doesn’t grant you the ability to “exit” or “break”. And there are various ways to do this.

First one is using MaybeT to stack on the IO Monad. Since it follows the monoid law, and its mzero denotes the “exit”. It is easy to understand just a little bit inconvenient. You have to lift other IO action into the Monad stack.

```
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as B

main = runMaybeT $ forever $ do
  s <- lift B.getLine
  when (s == “0”) $ mzero
  lift $ print s
```

One other way it to use the notorious Continuation Monad. Or more correctly, ContT to transform the IO Monad into ContT r IO. Obviously it is much more complicated than the MaybeT solution. You have to get your head around the callCC and call the passed-in exit action when you would like to jump out the loop.

```
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Cont
import qualified Data.ByteString.Char8 as B


main :: IO ()
main = flip runContT (\_ -> return ()) $ do
           callCC $ \exit -> forever $ do
                                      s <- lift B.getLine
                                      when (s == "0") $ exit ()
                                      lift $ print s
```

For online judge problem I would most likely use the first one. callCC style is more like to exploring the possiblities.
