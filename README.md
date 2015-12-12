# Haspot: A Hakyll blog generator with Casper theme as default

Haspot is a static site generator based on Hakyll, and it ported Ghost’s beautiful [Casper](https://github.com/TryGhost/Casper) theme. Once you install it, it is good to go.


## Installation 

First you have to install all of the dependencies. Most important one is hakyll.

``` bash
cabal sandbox init
cabal install
cabal exec watch
```

## Deployment

``` bash
cabal exec haspot deploy
```
