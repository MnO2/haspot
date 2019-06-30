---
layout: post
title: "Optimizing jieba-rs to be 33% faster than cppjieba"
date: 2019-06-30 19:29 
comments: true
categories: 
---
To start this blog post with a disclaimer: I admit that it is a clickbait title since every benchmark is a lie in its own, and I have no intention to start another programming language flame war. This blog post is mainly to share my experience on taking an emerging programming language's ecosystem seriously and evaluating it by working on a serious project, and see how far we can go in terms of performance and development experience.

The project I chose as mentioned in the title is [jieba-rs](https://github.com/messense/jieba-rs), the rust implementation of a popular Chinese word segmentation library: [Jieba](https://github.com/fxsjy/jieba). A quick background introduction about [word segmentation](https://en.wikipedia.org/wiki/Text_segmentation#Word_segmentation) for the readers who don't speak Chinese. Chinese and Japanese in their written scripts are only delimited up to the sentence but words are not delimited. For example, the following sentence could be literally translated to "Today's weather very good" in the same order, but you have no idea where the word boundaries are.

```
今天的天气真好
```

After the word segmentation, the sentence could be broken up as follows (space delimited):

```
今天(Today)  的('s) 天气(weather) 真(very) 好(good)
```

It's unlike English where spaces are added between words. This makes the natural language processing harder since most of the methodologies requires the granularity of words. Therefore the word segmentation is a core part when it comes to the tasks like search engine and language understanding. 

[Jieba](https://github.com/fxsjy/jieba) is the original implementation and it is implemented in python. It became so popular in the wild not only due to it strikes a good trade-off between the segmentation result and the performance. It may not be the most state-of-the-art or the most accurate word segmentation library but it's a very popular engine and got ported into different programming languages like [go](https://github.com/wangbin/jiebago) and the bindings to the [cppjieba](https://github.com/yanyiwu/cppjieba). Rust as a language focus on zero-cost abstraction, naturally I would like to benchmark the rust version against cpp version.

About a couple of weeks back I found that [messense](https://github.com/messense/) already had a good version of the rust implementation with the  maximum probability approach and the viterbi decoding finished. The code is well-written and self explainable therefore I decided to contribute and collaborate with messense. Even though I started following rust's development occasionally ever since it was in 0.4, I haven't catched up the latest 2018 edition, I started the contribution by implementing the left-out keyword extraction features using TextRank and TFIDF to get myself familiar with the language. In the meantime, I was curious how performant the rust implementation is, then I run the weicheng executable in the example folder, which is the same benchmark test used in cppjieba, by running the word segmentation on the weicheng script for 50 times and times the running time. I wasn't not satisfied with the result since the cppjieba runs about 5-6s on the following spec of Macbook.

```
MacBook Pro (13-inch, 2017, Two Thunderbolt 3 ports)
2.5 GHz Intel Core i7
16 GB 2133 MHz LPDDR3
```

However, jieba-rs at the time was running like 10s-ish. It was almost the double of the running time of cppjieba. 

With a couple of weeks of effort, now as the time of this blog post on June 30th, 2019. The running time of the jieba-rs is now around 33% faster than cppjieba as shown in the following.

Branch `darts` with the commit: ab2fbfe
```
➜  jieba-rs git:(darts) ./target/release/weicheng
4138ms
```

Branch `master` with the commit: a198e44
```
➜  jieba-rs git:(master) ./target/release/weicheng
4330ms
```

Branch `master` of cppjieba with the commit: 866d0e8
```
➜  build git:(master) ✗ ./load_test
process [100 %]
Cut: [6.219 seconds]time consumed.
process [100 %]
Extract: [0.679 seconds]time consumed.
```

You can see that 4138ms / 6219ms is roughly 66.5% of the running time, with the double array trie implementation adopted (not yet merged into the master).

I would also like to mention the [rust-darts](https://github.com/andelf/rust-darts) implementation finished by [andelf](https://github.com/andelf/). He helped a lot on updating the three-year-old library so that I could import the library into jieba-rs to try it out. It's on my roadmap to use DARTS as the underlying data structure once the required [features](https://github.com/andelf/rust-darts/issues/24) are implemented.

Now, I'd like move on to the key steps I did to reduce the running time as much as possible.


### Identifying the bottleneck with Instrument
The first step of performance tuning is to measure it and identifying the critical sections so you could put your efforts at the slowest part. The write-up from [siddontang](https://github.com/siddontang) about [optimizing TiKV on Mac](https://www.jianshu.com/p/a80010878def) is very helpful. By recoding the stack traces of the weicheng binary, it is easy to tell that the program spends its time on two parts: Regex and Viterbi Decoding.

### Regex
[BurntSushi](https://github.com/BurntSushi)'s regex library is very performance in general since it's executed in DFA. The [documentation](https://github.com/rust-lang/regex/blob/master/PERFORMANCE.md) also marked it clear where you should be looking at when the performance is the consideration you have. 

```
Advice: Prefer in this order: is_match, find, captures.
```

A few lines of [changes](https://github.com/messense/jieba-rs/commit/3d013211f3d76d00680f6f670c4f96b808f43571) from using Captures to Matches reduce the running time by 2s.

### Using dynamic programming to trace the best path in Viterbi decoding
I have speech processing and natural language processing background so I am quite familiar with hidden markov model training and viterbi decoding. It used to be just a midterm course assignment when I was the TA of the course. I am pretty sure the viterbi decoding and path reconstruction could be done in two two-dimension matrices. In the original python implementation it has done extra memory allocation by copying the best path so far, where actually you only need to track it by a prev matrix to remember the last state. 
[changing this](https://github.com/messense/jieba-rs/commit/2d1418092f595a3799d2d90f7a314b6855898261) earned us another 2s of speedup.

### Move memory allocation out of the loop

With the convenient Vec and BTreeMap provided in the standard library, we often forget there are actually expensive memory allocation hidden behind the abstraction. The word segmentation is basically a task where you are running a main loop to loop through the character stream to see if it could be broken up here. By allocating the memory outside the loop would obviously boost the performance, and with the handy resize you only need to reallocate when you need more memory.

### Remove allocation as much as possible

Transient Vec storage is convenient and it could make the code more readable, but when it comes to the scale of milli-second, it would be better to rethink if you really need them. Some cases you only need to track the states by one or two variables.


### SwissTable is damn fast

Right now we are using HashMap to store the dictionary and all of the prefixes of those words on the master branch. We know the HashMap in the standard library now is the hashbrown implementation, where it is based on Google’s Swiss Table. You can watch the introduction talk Google Engineer gave at 
[CppCon 2017](https://www.youtube.com/watch?v=ncHmEUmJZf4), it’s very inspiring on the approaches. SIMD instructions like SSE was used and therefore it’s damn fast when you are using Intel’s CPU. Even with the naive iteration through the prefixes of the strings to construct the DAG, it’s still performant since the scalar is so small. A theoretical better algorithm but bigger scalar absorbed into the big-O could perform far worse than naive approaches with SwissTable.


### Adopting Double Array Trie.
There has been [researches](http://www.hankcs.com/program/algorithm/double-array-trie-vs-aho-corasick-double-array-trie.html) comparing aho-corasik and double array trie 's speed with respective the maximum forward-matching word segmentation algorithm, and the conclusion was double array trie performs better in the case of Chinese word segmentation due to the data access pattern. I tried it out both and the conclusion matches, at least for the implementation we have as the time of June 2019.

For aho-corasick I was using the library implemented by BurntSushi, so I assumed it is the best in class. It performs much worse than DARTS, therefore I briefly looked into the code base, it seems that there are indirection on whether to use NFA or DFA when [the methods are called](https://github.com/BurntSushi/aho-corasick/blob/master/src/ahocorasick.rs#L1001), I am not sure this is the main reason resulting into the significant slow down though.

For DARTS implementation I am using the one ported by [andelf](https://github.com/andelf/) from [darts](http://chasen.org/~taku/software/darts/) in C++. Its code is much easier to read than libdatrie though lacking the features of dynamic insertion and deletion and tail compression. I am intended to add those features into the [rust-darts](https://github.com/andelf/rust-darts). 

## Lesson Learned and Afterword
Over the course of performance tuning and development I am convinced that Rust ecosystem has its potential and it seems to get a lot momentum in 2019, with so many Rustcons host around the world. Its spectrum of libraries is still far from the richness and matureness where you can find in C++ and Python world, but the standard library and a few core libraries contributed by star developers are very high quality and you could learn a lot by merely reading their code base. And its affine type system makes you feel safe to do a major refactoring for the performance tuning etc since you know compiler would call you dumb and catch those errors. If you are careless about a few of the details, you could still easily spit out poorly performing code. However, it's also easy for you to improve it once you gain some experience on performance tuning and know where you should look at.

Specially thanks again the [messense](https://github.com/messense/) and [andelf](https://github.com/andelf/) on the quick response to the pull requests and reviews. It's a pleasant experience to work on these projects in Rust with good developers.
