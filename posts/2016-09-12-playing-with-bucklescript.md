---
layout: post
title: "Playing with Bucklescript"
date: 2016-09-12 17:30
comments: true
categories: 
---
As of the time of 2016, most of the people agrees that Javascript is truly a
language could be run anywhere. The pending problem to be solved is how to
manage large Javascript projects. A few of industrial strength projects like
Angularjs made the leap and turned to what Microsoft has proposed: Typescript.
It’s quite obvious that it would be type-safe and easier to refactor by
introducing a type system. Also, it is an optional typing system and therefore
it works non-instrusively to your existing projects. You can gradually introduce
Typescript’s type declaration file to get you the type-safe. Microsoft also has
invested a lot on Typescript’s toolchain to make it work in Visual Studio Code
and IDE. For the case that you need a production-ready and hiring-easy
toolchain, Typescript is basically a no-brainer choice. Even so, I am still
quite unsatisfied with all of the gotchas in Typescript inherited from
Javascript in order to be 100% back-compatible. For the use cases that allows me
to pick my language, I am still inclined to go for static-typed languages
compiled to Javascript, with good Javascript interoperability.

There are a few options on the table that looks mature enough for use.

1.  Ghcjs: Althouh officially it is still not 1.0, it seems to be good to use for
some middle-size project. However, the downside is also obvious. It is the
monstrous size Javascript with all of the Ghcjs runtime. a few lines of code
could generate a few MB of target file. Maybe it is OK for the project where you
rarely need to upgrade, then you don’t need to download the target Javascript
very often. A famous use case is Plow Technology using Ghcjs for its dashboard,
where I think it is a fair use case. The other downside is it is hard for
Javascript world to call Haskell world, by the hindrance of the Generics and
runtime. The good is that you can code in an extremely expressive static typed
language with all of its ecosystem on Hackage.
2.  Purescript: A Haskell-like language with is strict-by-default and almost
everything in Haskell98 are included. It’s quite well-developed given that it is
purely work from community without the support of companies. Completely new
language and with strict semantic means it cannot leverage the ecosystem of
Haskell and it has to do everything to be battery included. Since it’s release
it has grown to the size that is usable and it’s interoperability with
Javascript looks sound. Also the compiled Javascript is readable (though
interleaved with the magic word of Monad in between). It worths considering if
the project members all know Haskell.
3.  Elm: The posterchild of Prezi and an experimentation on the feasibility of FRP.
In the latest version it was overthrown and FRP interface was abandoned and
turned to traditional Model-View-Controller. This is a big downer since it
more-or-less showing you would be better not to rely on this project since it
might introducing breaking changes and you are screwed. Apart from that it looks
quite nice, The language is Haskell-like, but not only a language, it also comes
with its infrastructure to make the composition of the program easier, Reactjs
was inspired a lot from Elm, if you know Reactjs you can find a lot of common in
their design.

Here comes today’s main character: Bucklescript. I read the 1.0 release news
from Chinese quora: Zhihu. The creator of Bucklescript: Bob Zhang post a brief
introduction of the language and target. I was intrigued by its compiled result.
It looks super readable even more than Purescript, but at the same time support
almost all of the OCaml ecosystem libararies to compiled to Javascript. It is
derived from js_of_ocaml but heavily inspired by Typescript. It made me really
would like to pick-up OCaml and play with it by a few toy example programs.

Due to a recent requirement I really need a Slack bot and I think it might be a
good use case to test its interoperability with Javascript. What I use for Slack
is Slack’s official nodejs library. Bascially you have to require the library
and new an client object and using “on” method to listen on various events. And
the events are defined as different attributes of the object.

Here is the toy program

```
    type rtmclient 
    external create_rtmclient: string -> rtmclient = “RtmClient” [@@bs.module “
    ”, “RtmClient”][@@bs.new] 
    let client = create_rtmclient “ABCDEFG”
    external start_client: rtmclient -> unit = “start” [@@bs.send]

    type rtmevent
    external message: rtmevent = “RtmClient.RTM_EVENTS.MESSAGE” [@@bs.val]
    external channel_created: rtmevent = “RtmClient.RTM_EVENTS.CHANNEL_CREATED” [@@bs.val]
    external on: rtmclient -> rtmevent -> (string -> unit [
    ]) -> unit = “” [@@bs.send]

    let handler: string -> unit [
    ] = fun [
    ] s -> Js.log(s)
    let () = on client channel_created handler 
    let () = on client message handler 
    let () = start_client client
```

And its compiled javascript

```
    // Generated by BUCKLESCRIPT VERSION 1.0.1 , PLEASE EDIT WITH CARE
    ‘use strict’;

    var RtmClient = require(“
    ”);

    var client = new RtmClient.RtmClient(“ABCDEFG”);

    function handler(s) {
     console.log(s);
     return /* () */0;
    }

    client.on(RtmClient.RTM_EVENTS.CHANNEL_CREATED, handler);

    client.on(RtmClient.RTM_EVENTS.MESSAGE, handler);

    client.start();

    exports.client = client;
    exports.handler = handler;
    /* client Not a pure module */
```

To be honest it looks nice for the compiled result, even you claimed that it is
hand-written I think most of the people would believe you. Just that the
document is still young and it cost me some time to figure out how to map the
Javascript attribute into the OCaml world. Also, learning the magic of “@bs”
attribute, especially the higher order use case took me some time to figure it
out. As long as you mark the function with “@bs” then it is no longer the same
type as the normal function, therefore you can not simply pass a lambda function
to the Javascript world callback, you must mark it with “@bs”. And I still
cannot get the hang of bringing values defined in Javascript world into OCaml’s
polymorphic variants, I tried “@bs.as” but it didn’t work at all. Not sure if I
missed something in the syntax.

Overall, I liked the language, not sure if it is officially supported by
Bloomberg or just Bob’s side project derived from work. It also gave me a chance
to improve my OCaml skill, which is a pending item in my Todo list. Now it is
time to cross it off.

