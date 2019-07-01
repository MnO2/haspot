---
layout: post
title: "最佳化 jieba-rs 中文斷詞性能測試 (快于 cppjieba 33%)"
date: 2019-07-01 15:36 
comments: true
categories: 
---

昨晚寫了一篇關於最佳化 [jieba-rs](https://github.com/messense/jieba-rs) 英文的[介紹](https://blog.paulme.ng/posts/2019-06-30-optimizing-jieba-rs-to-be-33percents-faster-than-cppjieba.html)，但想說 jieba 的使用者多半還是在中文圈，對於宣傳來講 hacker news 跟 reddit 可能無法觸及到真正會使用的使用者，於是為了宣傳，也是為了讓 search engine 可以搜尋到，就來把性能的部分另外寫成中文的一篇。關於過程我就不再重新用中文再寫一次了，實在太累人了。有興趣的人可以閱讀[英文版](https://blog.paulme.ng/posts/2019-06-30-optimizing-jieba-rs-to-be-33percents-faster-than-cppjieba.html)

測試機器的機器規格如下
```
MacBook Pro (13-inch, 2017, Two Thunderbolt 3 ports)
2.5 GHz Intel Core i7
16 GB 2133 MHz LPDDR3
```

測試過程仿照[結巴(Jieba)中文分詞系列性能評測](http://yanyiwu.com/work/2015/06/14/jieba-series-performance-test.html)所描述，先一行一行讀取檔案圍城到一個陣列裡，然後循環 50 次對圍城每行文字作為一個句子進行斷詞。 分詞算法都是採用精確模式，也就是包含了 HMM 的部分。

耗時的資料如下，從高到低排序

實作                      耗時            版本 . 
--------------------  -----------    --------------
cppjieba                 6.219s         866d0e8 
jieba-rs (master)        4.330s         a198e44 
jieba-rs (darts)         4.138s         ab2fbfe 

以上耗時都是計算斷詞過程的耗時，不包括字典載入的耗時。

這篇會著重於評測只是為了宣傳，並不想陷入語言之爭，這也是我英文版有寫主要是分享關於用 Rust 最佳化的經驗，也是為了我自己衡量可以在工作中多認真使用 Rust 為目的。
