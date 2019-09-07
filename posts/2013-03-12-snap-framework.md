---
layout: post
title: "Snap Framework Tutorial Part 1 — The Design of Snap Framework"
date: 2013-03-12 17:30
comments: true
categories: 
---
### Motivation
Participating in Taiwan’s g0v Open Data community, favonia, miaout17 and I wrote a web app to assist the human laboring work in mapping from CNS 11643 encoding to Unicode. That is a sub-project of moedict, which aims to transform the data crawled from the Ministry of Education’s website into machine readable format. At the same time, build an easy-to-use demo site to promote the advantage of Open Data. Aside from accomplish our task, we would also like to dabble in the technologies for building website in Haskell community, to get some ideas about how it feels like to develop web app in Haskell. After some studies, we decided to use snap framework for implementing our backend logic. For front end, we use clay for css and coffeescript for browser scripting. To deploy the compiled web app, we use keter. Due to the lack of tutorial or examples (comparing to those well-documented project) for the technologies we used, I’d like to publish a series of write-up introducing them. I’ll start from snap framework. For people who would like to see our code, our project’s source code is here

### The Design of Snap Framework
Haskell has a powerful type system. In Haskell 98 it can already describe a lot of things other main stream programming languages can’t describe. With GHC’s extension much more can be done. However, snap framework’s goal is to strike a balance between type safety and easy adoption for a new comer. And I personally think snap did very well to achieve this goal. For snap and snap-core, it’s tailored only with Lens and Monad as of version 0.11. Only when you have to dive into the http server part, you have to deal with iteratee. But I believe with the release of io-streams, iteratee will be replaced for better handling with async exception etc. So let’s look at how snap framework model a web app with Monad and Lens.

### How to Model a Web App with Monad and Lens
When writing in haskell, it is very important to think from the aspect of types, and the relationship between them. It’s like composing electronic circuits or playing with LEGO. To model an executing web app, you can squeeze your eyes a little bit, and you can see it look like this:

![](/images/legacy/0*AEz8hJnHwaf0Hw6q.png)

Here we call a web app as a service. To run it, usually it at least consists of Config and State. However, if we want to modify state in haskell, we need the help of setter and getter, which together means Lens.

![](/images/legacy/0*RpZ2WT8GFJZkUDQW.png)

For a real world web app, one service is far from enough. Also, categorizing the whole web app’s state into one service is not considered type safe. To accommodate this problem, snap framework consider a web app as a tree of services. For example, your web app may need to communicate with database service and OAuth service and template rendering service to serve a http request. And we also want to have some kind of permission control between the services. After all, you wouldn’t like your template rendering service mess up with the state of OAuth service. Hence, snap framework also take a chroot like permission control into account, and incorporate it into type system.

![](/images/legacy/0*P0Yl9ztxv4Msrq5m.png)

How the type is expressed in snap framework? Given a service with state v runs in a Reader plus State Monad Stack and outputs a value in the end, another b state is added to the type to denote the base state. It could be writing like m b v a. Base state is like global variable shared by several services. Or take file system as an analogy. Base state is any data on the path of current directory up to the root. Say we are in “/abc/def/ghi”, then this service can get and set any data in “/abc/“ and “/abc/def/“ and “/abc/def/ghi/…” . Setting base state is as to chroot to different directories. If we set base state to equal to v, then we are chrooting to “/ghi/”.

Now let’s look at how they are really defined in code. An instance of MonadSnaplet corresponds to our “service” mentioned above. It’s type seems daunting, but with the explanation above, it would easier to understand.

```
class MonadSnaplet m where
	with :: SnapletLens v v’ -> m b v’ a -> m b v a
	withTop :: SnapletLens b v -> m b v’ a -> m b v a
	with’ :: SnapletLens (Snaplet v) v’ -> m b v’ a -> m b v a
	withTop’ :: SnapletLens (Snaplet b) v’ -> m b v’ a -> m b v a
	getLens :: m b v (SnapletLens (Snaplet b) v)
	getOpaqueConfig :: m b v SnapletConfig
```

We have with and withTop here. For with, it’s type is saying given getters and setters and we are chrooting to the state of current service, we run some monad and modify the state of this service. As for withTop, it says given getters and setters, we chroot to b state, which could be far above the tree or just direct parent, we are modifying the state of this service while possibly referencing other services’ states.

A Snaplet is basically the collection of configurations for a service, and the service’s value itself.

```
data Snaplet s = Snaplet
{
	_snapletConfig :: SnapletConfig
,	_snapletValue :: s
}
```

As for the detail of SnapletConfig, we can see that it contains routing information (scRouteContext,scRoutePattern), information about this service (scId, scDescription, scFilePath) and its location in the tree (scAncestry).

```
data SnapletConfig = SnapletConfig
{
	_scAncestry :: [Text]
,	_scFilePath :: FilePath
,	_scId :: Maybe Text
,	_scDescription :: Text
,	_scUserConfig :: Config
,	_scRouteContext :: [ByteString]
,	_scRoutePattern :: Maybe ByteString
,	_reloader :: IO (Either Text Text)
}
```

And here we use ALens’ for Lens, basically you can think of it as the most simple Lens introduced in here. That is, it is getter and setter of some state Snaplet a contained in s.

```
Type SnapletLens s a = ALens’ s (Snaplet a)
```

We have introduced the running status of these services, but we have to initialize them at the very first, right? Hence we would like to introduce another important monad in snap framework: Initlializer monad.

```
newtype Initializer b v a =
	Initializer (LT.LensT (Snaplet b)
				     (Snaplet b)
				     (InitializerState b)
				     (WriterT (Hook b) IO)
				     a)
```

Again, it’s type seems daunting. To explaint it we have to look at the definition of LensT.

```
newtype LensT b v s m a = LensT (RST (ALens’ b v) s m a)
```

LensT basically is an RST (Reader and State) monad built on the underlying m monad, with a output value, and ALens’ b v reader config and s as its state. With this interpretation, let’s go back to the definition of Initalizer monad, we can see that Initializer monad is

```
LensT (RST (ALens’ (Snaplet b) (Snaplet b)) (InitializerState b) (WriterT (Hook b) IO) a)
```

It’s a monad with reader ALens’ (Snaplet b) (Snaplet b) and state InitializerState b, run on a writer monad WriterT (Hook b) IO) and produce value a. Where InitializerState b is just another collection of information used in the process of initialization. It establishes routing path to Handler info, and cleanup function, and initialization message etc.

```
data InitializerState b = InitializerState
{
	_isTopLevel :: Bool
, 	_cleanup :: IORef (IO ())
,	_handlers :: [(ByteString, Handler b b ())]
, 	_hFilter :: Handler b b () -> Handler b b ()
, 	_curConfig :: SnapletConfig
,	_initMessages :: IORef Text
,	_environment :: String
}
```

And the underlying writer monad is using Hook as the transformation function. It may produce some error message in Text and has side effect, but finally produce Snaplet.

```
newtype Hook a = Hook (Snaplet a -> EitherT Text IO (Snaplet a))
```

Now we are done with Initialization. It’s time to move on to the most important monad: Snap monad. It is the monad to model the process of receiving request and produce responses. It is a State monad runs on http server’s Iteratee monad, producing SnapResult in the end.

```
Newtype Snap a = Snap {
	unSnap :: StateT SnapState (Iteratee ByteString IO) (SnapResult a)
}
```

Because it is processing request, in SnapState we must have to access request, response and error logs. It may also modify the time out duration for each request.

```
Data SnapSate = SnapSate
{
	 _snapRequest :: Request
	,_snapResponse :: Response
	,_snapLogError :: ByteString -> IO ()
	,_snapModifyTimeout :: (Int -> Int) -> IO ()
}
```

In order to be able to extend the system, we define MonadSnap to lift Snap monad to some other monad as well.

```
class (Monad m, MonadIO m, MonadCatchIO m, MonadPlus m, Functor m, 
	Applicative m, Alternative m) => MonadSnap m where
	liftSnap :: Snap a -> m a
```

Therefore, for a Handler dealing with requests, it is equal to the process of running in Snap monad, transforming base state Snaplet b and state of this service Snaplet v into another state. The Handler’s definition is as follows,

```
newtype Handler b v a =
	Handler (L.Lensed (Snaplet b) (Snaplet v) Snap a)
```

With the definition of Lensed,

```
newtype Lensed b v m a = Lensed
{
	unlensed :: ALens’ b v -> v -> b -> m (a, v, b)
}
```

We can rewrite Handler’s definition:

```
(ALens’ (Snaplet b) (Snaplet v)) -> (Snaplet v) -> (Snaplet b) -> Snap (a, (Snaplet v), (Snaplet b))
```

It is clear that it is a function wehn given getter/setter, tranforms current service’s state and base state into newer state. Because it runs in Snap, with Lens it can access to information of request and response.

Conclusion
We didn’t list any hello world like web app here. Because without understanding the type structure of snap framework, it would be hard to get far enough to write your own web app of any interest. For any toy example, the documentation on the official website is clear and we won’t repeat them here.
