---
layout: post
title: "Hakell Library Survey: Async"
date: 2012-06-25 17:30
comments: true
categories: 
---
My friends and I have been working on a haskell binding to libpulse, in the hope to provide a much thorough API than pulse-simple, which only bound the limited simple API but not the async API Due to the innate asynchronous architecture of libpulse and the emulated synchronous interface we want to provide to the users, we have to deal with a lot of dirty jobs with respect to asynchronous behaviours. We did some surveys and here is a little memo I decide to write it down.

Suppose you want to concurrently download the content of two links, take whatever return first and drop the other one. How would you implement this in Haskell? One way is to implement a select in haskell, the select is named after the select from POSIX

```
import Control.Concurrent
import Control.Concurrent.STM
import Network.Curl.Download
import qualified Data.ByteString.Char8 as B

downloadThread :: String -> TMVar B.ByteString -> IO ()
downloadThread url tmvar = do
    page <- openURI url
    case page of 
        Left s -> return ()
        Right c -> atomically $ putTMVar tmvar c

select :: [TMVar a] -> STM a
select = foldr1 orElse . map takeTMVar

main = do
    tmvar1 <- atomically $ newEmptyTMVar
    tmvar2 <- atomically $ newEmptyTMVar
    forkIO $ downloadThread "http://www.google.com" tmvar1
    forkIO $ downloadThread "http://www.yahoo.com" tmvar2 
    page <- atomically $ select [tmvar1, tmvar2]
    B.putStrLn page
```

With the help of TMVar, we can wait for the completion of the forked jobs, hence we can emulate a synchronous behaviour building on the top of this.

When you come up with an idea, a high probability is it have been already implemented by somebody else and been uploaded to hackage. Here is an interfance provided by async, which is implemented by Simon Marlow

```
import Network.Curl.Download
import Control.Concurrent.Async
import qualified Data.ByteString.Char8 as B

downloadThread :: String -> IO B.ByteString
downloadThread url = do
    page <- openURI url
    case page of 
        Left s -> error s
        Right c -> return c

main = do
    withAsync (downloadThread "http://www.google.com") $ \a1 -> do
    withAsync (downloadThread "http://www.yahoo.com") $ \a2 -> do
    res <- waitEither a1 a2
    case res of
        Left page1 -> B.putStrLn page1
        Right page2 -> B.putStrLn page2
```

The package adds a thin layer over the concurrency operations provided by Contrlo.Concurrent. It defines type Async to provide some safety. Basically it works the same way underlying, using STM for synchronization and using fork# instead of forkIO for optimization.

Some disadvantages of this library is that it is tightly-coupled with IO, if you want to define MonadIO, it is not currently supported. It also uses lightweight thread, so in the case of libpulse, which uses OS threads on its own. Interacting with it through foreign function interface would be a problem.

If you are interested in our project, you can fork us on pulse. It has not completed yet, but we devoted a lot of time working on it. Hope soon it would be in beta stage.
