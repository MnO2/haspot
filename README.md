# Haspot 
Haspot is a static site generator based on Hakyll, and it ported Ghost’s beautiful Casper theme. Once you install it, it is good to go.

## Installation 

First you have to install all of the dependencies. Most important one is hakyll.

```
cabal sandbox init
cabal install --only-dependencies
cabal configure
cabal build
cabal run watch
```

## Deployment


### Github page
```
cabal run deploy
```

