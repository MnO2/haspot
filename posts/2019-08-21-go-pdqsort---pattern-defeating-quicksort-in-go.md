---
layout: post
title: "go-pdqsort - Pattern Defeating Quicksort in Go"
date: 2019-08-21 22:03 
comments: true
categories: 
---

[go-pdqsort](https://github.com/MnO2/go-pdqsort) is my implementation of pattern defeating sort in golang. I knew about pattern defeating sort from rust's standard library documentation. I've never heard about this algorithm and it immediately intrigues my interest. I googled about it and found the original [implementation](https://github.com/orlp/pdqsort) and their discussion threads on [hacker news](https://news.ycombinator.com/item?id=14661659) and [reddit](https://www.reddit.com/r/cpp/comments/2z6hgx/patterndefeating_quicksort/). Then I read the [technical report](https://paperpile.com/view/c3820723-6d9c-0a86-ab02-8e46dc22aec5) from Orson Peters. The key observation from the algorithm is that merely reducing the miss rate from CPU branch prediction, it would make the sorting speed greatly improved (no need to flush the cache line etc), so the technique adopted was to put the index of the array in the buckets and don't swap them immediately, but put them in the right bin and swap them in one batch at the end of the loop. This works perfectly in the language  with zero cost abstraction like C/C++ and Rust. I wasn't sure about that would work in the heap-managed languages like golang, python and ruby etc, since most of the things are on the heap (with pointer indirection) and often results into cache misses, the impact of branch prediction miss rate might be neglectable. (For sure it would depend on the escape analysis in golang compiler, it would put the things on stack if it doesn't escape in general). Though my hunch was that it would be slower, it still a good practice to implement the algorithm by myself so that I would get to know the details of the algorithm. And here are the results

 implementation        speed
------------------  -------------    
 pdqsort             80874 ns/op     
 std-lib sort        69828 ns/op     

I am not sure about what std-lib's sort is, but it looks like introsort since it switches to heapsort when the recursion is too deep, and it incorporates the techniques from shellsort as well, but the main part is still Bently's quicksort technique. You can tell from the result that pdqsort is significantly slower than std-lib's sort, probably due to the overhead of those memory batch swap, where it triggers extra allocation or cache eviction, pretty much as expected.

If you are interested in the algorithm, you could check out the [code](https://github.com/MnO2/go-pdqsort) by yourself.
