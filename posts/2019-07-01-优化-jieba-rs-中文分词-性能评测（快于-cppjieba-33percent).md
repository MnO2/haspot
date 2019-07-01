---
layout: post
title: "优化 jieba-rs 中文分词性能评测 (快于 cppjieba 33%)"
date: 2019-07-01 14:10 
comments: true
categories: 
---
昨晚写了一篇关于优化 [jieba-rs](https://github.com/messense/jieba-rs) 英文的[介绍](https://blog.paulme.ng/posts/2019-06-30-optimizing-jieba-rs-to-be-33percents-faster-than-cppjieba.html)，但想说 jieba 的使用者多半还是在中文圈，对于宣传来讲 hacker news 跟 reddit 可能无法触及到真正会使用的用户群，于是为了宣传，也是为了让 search engine 可以搜索到，就来把性能的部分另外写成中文的一篇。关于过程我就不再重新用中文再写一次了，实在太累人。有兴趣的人可以阅读[英文版](https://blog.paulme.ng/posts/2019-06-30-optimizing-jieba-rs-to-be-33percents-faster-than-cppjieba.html)

测试机器的机器规格如下
```
MacBook Pro (13-inch, 2017, Two Thunderbolt 3 ports)
2.5 GHz Intel Core i7
16 GB 2133 MHz LPDDR3
```

测试过程仿照[结巴(Jieba)中文分词系列性能评测](http://yanyiwu.com/work/2015/06/14/jieba-series-performance-test.html)所描述，先按行读取文本围城到一个数组里，然后循环 50 次对围城每行文字作为一个句子进行分词。 分词算法都是采用精确模式，也就是包含了 HMM 的部分。

耗时数据如下，从高到低排序

实作                耗时        版本 . 
------------------  --------    ---------
cppjieba            6.219s      866d0e8 
jieba-rs (master)   4.330s      a198e44 
jieba-rs (darts)    4.138s      ab2fbfe 

以上耗时都是计算分词过程的耗时，不包括词典载入的耗时。

这篇会着重于评测只是为了宣传，并不想陷入语言之争，这也是我英文版有写主要是分享关于用 Rust 优化的经验，也是为了我自己衡量可以在工作中多认真使用 Rust 为目的。
