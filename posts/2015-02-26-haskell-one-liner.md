---
layout: post
title: "Haskell One Liners"
date: 2015-02-26 17:30
comments: true
categories: 
---

The scripting languages like Perl and Ruby are very well-known for expressing the data crunching task into one-liner. With their command line flags like -p, -e and default variable $_ with regular expression. The job just gets easier.
I believe functional languages are also good at one-liner, since lambda-like structure are good in data transformation. Here is a few I would do with Haskell.

### number each line of files
```
ghc -e "print . (zip [1..]) . lines =<< getContents"
ghc -e "print . (filter (/= '\n'))  =<< getContents"
```

### count lines
```
ghc -e "print . length . lines =<< getContents"
```

### print specific line
```
ghc -e "print . (filter (\x -> (fst x) == 1)) . (zip [1..]) . lines =<< getContents"
```

### Regular expression is relatively painful in Haskell. In order to use it you have to add the modules you would like to load in evaluation mode first.

```
:m +Text.Regex
```

Then hit with POSIX regular expression syntax.

```
ghc -e 'print . (map (\s -> subRegex (mkRegex "^[[:space:]]+") s "")) . lines =<< getContents'
```

I would add more if I can think of others.
