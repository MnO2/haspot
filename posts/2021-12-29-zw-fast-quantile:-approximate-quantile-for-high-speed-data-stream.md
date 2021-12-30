---
layout: post
title: "zw-fast-quantile: Approximate Quantile for high-speed data stream in Rust"
date: 2021-12-29 23:54 
comments: true
categories: 
---
A couple of weeks ago I exchanged messages with [ljw1004](https://github.com/ljw1004), where he was surveying fast quantiles implementation in rust. He conducted a thorough [benchmark](https://github.com/postmates/quantiles/issues/32) against the GK01 and CKMS algorithms in the [postmates/quantiles](https://github.com/postmates/quantiles) crate, in order to understand the tradeoffs among the error rate, memory usage and the update/query time. He mentioned the Zhang Wang algorithm in a [literature review](https://github.com/postmates/quantiles/issues/33), where I later found that it was actually adopted in the boosted tree implementation in [Tensorflow](https://android.googlesource.com/platform/external/tensorflow/+/6341b8975b7660244e1ca3003bfce5371f1fd167/tensorflow/core/kernels/boosted_trees/quantiles/weighted_quantiles_stream.h#30). I got intrigued and decided to roll my sleeves and give it a shot to implement the algorithm in rust

I spent a few days to read the [paper](http://web.cs.ucla.edu/~weiwang/paper/SSDBM07_2.pdf) and read the reference implementation done by [jasonge27](https://github.com/jasonge27/fastQuantile) in C++. The C++ implementation only implemented the fixed size approach in the paper but lack the generalized approach to support the online streaming update. I read the paper carefully and implement the generalized algorithm and then we have [zw-fast-quantile](https://github.com/MnO2/zw-fast-quantile) crate.

The naive benchmark with criterion shows that the update operation is 2.6x faster than GK01 implementation in [postmates/quantiles](https://github.com/postmates/quantiles)

```
zw unbound quantile update
                        time:   [60.780 us 60.855 us 60.936 us]
                        change: [-1.4032% -0.9510% -0.5005%] (p = 0.00 < 0.05)
                        Change within noise threshold.
Found 8 outliers among 100 measurements (8.00%)
  2 (2.00%) high mild
  6 (6.00%) high severe
```

```
gk quantile update      time:   [156.84 us 157.02 us 157.24 us]
                        change: [-0.1907% -0.0503% +0.0969%] (p = 0.50 > 0.05)
                        No change in performance detected.
Found 11 outliers among 100 measurements (11.00%)
  6 (6.00%) high mild
  5 (5.00%) high severe
```

and query operation is 1.5x faster than GK01
```
zw unbound quantile query
                        time:   [229.62 ns 230.16 ns 230.77 ns]
                        change: [+1.3422% +1.8105% +2.2504%] (p = 0.00 < 0.05)
                        Performance has regressed.
Found 11 outliers among 100 measurements (11.00%)
  3 (3.00%) high mild
  8 (8.00%) high severe
```

```
gk quantile query       time:   [350.21 ns 350.48 ns 350.76 ns]
                        change: [-0.4638% -0.3109% -0.1670%] (p = 0.00 < 0.05)
                        Change within noise threshold.
Found 8 outliers among 100 measurements (8.00%)
  1 (1.00%) low severe
  2 (2.00%) high mild
  5 (5.00%) high severe
```

It matches with the emprical study from the paper that it is able to achieve 200-300x speedup over GK01.

To benchmark the memory usage, by storing complete 100M values in the `Vec` and calculating the quantiles is using up `7812k` of heap, and with the error rate set to `0.01` the `zw-fast-quantile` is only using up `1947k` of heap. It looks like it fulfills the goal of approximate quantile by saving the storage size without sacrificing the precision too much.


Implementing `zw-fast-quantile` is a lot of fun and I learned a lot. By reading the review [paper](https://arxiv.org/pdf/2004.08255.pdf) now I understand we can categorize the approximate quantile algorithm by different use cases. They could be

1. Streaming Model: Your data would arrive one by one and you might not know the size of your stream beforehand. You should be able to calculate the result in one pass without blowing up the memory. You might also improve the algorithm complexity if you know that you just gonna query the quantiles for the trailing N values.
2. Distributed Model: where you have a network of nodes, each of them have some values. However, the communication cost is high and you would like to avoid the communication cost as much as possible.
3. Update Time: In the real-time application like video/audio streaming, your data update would be very fast and it requires you the low latency on your update time. You would like to strike a balance between update time and the space complexity and accuracy.
4. Data Skewness: The famous t-digest would leverage on the fact where the error percentage is more imporatnt to the 99th percentile than 50th percentile. By leveraging the data skewness you can invent another set of algorithms.

`zw-fast-quantile` is falling on the category 3, and it is good to know that it influenced the implementation in Tensorflow's boosted trees. It could be another handy algorithm put in your toolbox and use it when the use case is right.

