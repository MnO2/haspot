---
layout: post
title: "The fallacies and numbers developers should know"
date: 2019-05-20 16:30
comments: true
categories: 
---
Oftentimes, you could tell a developer is good or not by talking with him if he/she is aware of the common fallacies appear in Unicode and Time, or distributed systems. After all, the growth of a developer is to understand the complexity of the world and the limitation of the abstraction, so the mental model fit better to how the real world is working over time. This blog post is try to curate a list where I find it is good, especially for junior developers. There is already 
[awesome falsehood](https://github.com/kdeldycke/awesome-fAalsehood) on github, but I don't like every link provided in the wiki. And it missed some parts where they are not fallacies, but the numbers you'd better to know.

## Latency
Let's start first by knowing the [latency charts](https://people.eecs.berkeley.edu/~rcs/research/interactive_latency.html) you should be aware of. This would give a sense of relativity when you analyze a program and to know which part it should be optimized for, given different kind of situation. In a distributed system, network latency dominates the share of the total time it spends. However, in a compute-intense application, you should be aware of how to optimize for cache-line hit, and to avoid the contention for cache slots.

## Distributed System 
The second one is the famous [fallacies of distributed system](https://en.wikipedia.org/wiki/Fallacies_of_distributed_computing). They are
* The network is reliable
* Latency is zero
* Bandwidth is infinite
* The network is secure
* Topology doesn't change
* There is one administrator
* Transport cost is zero
* The network is homogeneous

This is especially useful when you migrate your monolith application to be several components, or even further, to be micro services working together. Some of the naive function calls need to be extended with further assumptions (or the type would be changed if there is any). The call would need a retry and it would fail if it doesn't finish within a budget time, and you need to cancel it etc.

## Unicode
Unicode has grown to a beast where most of the junior developers misunderstand. There are numerous gotchas, like [the common string comparison problem in Turkey when you set the wrong locale](https://blog.codinghorror.com/whats-wrong-with-turkey/) to [gotchas that developers should know](https://nukep.github.io/progblog/2015/02/26/some-gotchas-about-unicode-that-every-programmer-should-know.html). I think Apple has done a good job on documenting and design the swift programming language in terms of the unicode support, and by explaining the concepts in the [document](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html).

## Time
One thing the awesome-falsehood did well is by collecting a list of blog posts that explain the time and calendar in programming well. It ranges from [storing UTC is far from enough for future date.](https://codeblog.jonskeet.uk/2019/03/27/storing-utc-is-not-a-silver-bullet/), [to the falsehood that you think it holds for unix time](https://alexwlchan.net/2019/05/falsehoods-programmers-believe-about-unix-time/). These are the blog posts that I personally read from the list that I think it is good, but not each of them

* [UTC is enough for everyone, right?](https://zachholman.com/talk/utc-is-enough-for-everyone-right)
* [Explanation of the critics on falsehood to Time](https://gist.github.com/thanatos/eee17100476a336a711e)

Also, Apple's document on NSCalendar also has some golds in it.
* [NSCalendar document](https://developer.apple.com/documentation/foundation/nscalendar)<Paste>
