---
layout: post
title: "cedarwood: Efficiently-Updatable Double Array Trie in Rust"
date: 2019-07-14 18:26 
comments: true
categories: 
---

In an effort to [speed up jieba-rs](https://blog.paulme.ng/posts/2019-06-30-optimizing-jieba-rs-to-be-33percents-faster-than-cppjieba.html), an efficient implementation of trie is needed in order to satisfying the following needs.

* To be able to list the prefixes that exist in the dictionary with a given string. For example, given the dictionary to be `["a", "ab", "abc", "z", "xz", "xy"]` and the input string `abcdefg`. We should be able to efficiently list the prefixes `["a", "ab", "abc"]` that exist in the dictionary.
* Due to the support of dictionary provided dynamically from the user, we need to support dynamic insertion into the data structure and at the same time still get us efficient common prefixes operation.
* To be able to efficiently tell if the given string is in the dictionary or not.

As mentioned [previously](https://blog.paulme.ng/posts/2019-06-30-optimizing-jieba-rs-to-be-33percents-faster-than-cppjieba.html) that aho-corasick performs quite slow from my brief testing, I didn't spend extra time to dig out why and I just accepted it. My focus was turned to refine the double array trie implementation, where at the time was [rust-darts](https://github.com/andelf/rust-darts). It performs quite well on the reading operations but due to its implementation was based on the [darts](http://chasen.org/~taku/software/darts/), a C++ implementation of static double array trie. It failed the requirement 2 mentioned above. Either we have to drop the API and getting farther away from the original python's implementation of Jieba, or we have to find alternatives to support dynamic insertion. I did contribute to the `rust-darts` with [dynamic insertion](https://github.com/andelf/rust-darts/pull/25), it resolves the feature lacking issue but the speed is still quite slow. It would make the loading time and testing time for jieba to be slow as well. Then I implemented the techniques mentioned [here](https://linux.thai.net/~thep/datrie/#Alloc) to make the trie building time much faster (from 30+ seconds down to around 5 seconds on my machine. It is much much better but I still think if we can do any better. To be fair to the `darts` implementation, the vanilla double array trie was invented by Aoe in the 1989 paper: [An efficient digital search algorithm by using a double-array structure](https://ieeexplore.ieee.org/document/17222) with mostly read operation in mind. It did support the update operations but it also said the update speed is quite slow. There are a few techniques to speed the update operations up and I already implemented [one of them](https://linux.thai.net/~thep/datrie/#Alloc) for the clean build.

## Look for better solutions

I googled around to see if there are any better solution, then I found [HAT Trie], which seems to be the state of the art implementation of the trie, with the cache in consideration. It is based on the paper published by Askitis Nikolas and Sinha Ranjan: `HAT-trie: A Cache-conscious Trie-based Data Structure for Strings.`. And it is good that [Tessil](https://github.com/Tessil) already had [C++ implementation](https://github.com/Tessil/hat-trie) on github with [very detailed benchmark comparisons](https://github.com/Tessil/hat-trie/blob/master/README.md). However, when I looked at the interface provided it seems like it only supports finding the longest prefix, and iterating through the string predictions in the dictionary matching a given prefix. It doesn't provide the interface for the requirement 1. I looked into the [code](https://github.com/Tessil/hat-trie/blob/master/include/tsl/htrie_hash.h#L1574) and it seems that it could potentially exposed from the implementation to support that, just that I am not familiar enough of the algorithm to do that. The concept of HAT-trie is also based on hash table with the concept of "burst" is unfamiliar to me, and since it is stored in unordered, my hunch stopped me from investing my time further to read the not-so-simple algorithm. Though the hindsight now is that the operation I need is not based on the requirement of the order in keys but the traversal of the trie, which should be supported by the longest prefix operation, just need to change the implementation. I would do that if I have more time. 

## Cedar and Cedarwood
The main character in this post is actually one of the candidates listed in the benchmark provided by Tessil. An double array trie implementation named [cedar](http://www.tkl.iis.u-tokyo.ac.jp/~ynaga/cedar/) caught my eyes, since its update seems to be very efficient, it is comparable with the other fast implementations in the list. And it's read access speed is also on the same ballpark with HAT-trie in the Dr. Askitis dataset, where the median key length is shorter than what is in the wikipedia title dataset. It definitely worths a closer look to me. The read access speed seems to be slower than HAT-trie in the long key length cases but for the Chinese segmentation scenario, the shorter key seems to be the case. We are not working on a query completion use case anyway. The author of cedar is [Naoki Yoshinaga](http://www.tkl.iis.u-tokyo.ac.jp/~ynaga/). He pointed us to the paper of `A Self-adaptive Classifier for Efficient Text-stream Processing` for further reading on his webpage but it is mostly an NLP paper but not a data structure paper, it only has one or two paragraphs describing the working on his improvement on double array trie. In the paper he cited another one, but you couldn't find the paper on any English website, even scihub. It turned out it is a paper in Japanese and I found the pdf on the Japanese website with the title of `タブル配列による動的辞書の構成と評価`. Even though I can understand basic to intermediate Japanese, it still didn't address that much into the detail in that Japanese paper. Therefore I decided to read the code directly.

It is a header-only C++ implementation and the code is very much shortened to reduce the header size, it is generally ok to read with a C++ formatter from IDE or VSCode, but it also took me a while to get the core concept behind it on improvement techniques due to the lack of comments. And with the `cedarwood` rust porting implementation, I added much more comments so it should be much easier to understand how it is working, but it is a required reading to read the original double array trie paper so that at least you know how the `base` and `check` works in double array trie cases. The following I'll briefly talk about the concept behind the skills.

## The Key Concepts in the Algorithm 
The inefficiency in the update operation of the vanilla double array trie is caused by the free slot searching. The original paper simply implies that you could use brute-force approach to iterate through the index in `check` and see if they are marked as owned, and iterate through until the location where you have the free slots distribution exactly match what you want (suppose you are relocating an existing trie node to a new free location). The technique specified [here](https://linux.thai.net/~thep/datrie/#Alloc) is basically leveraging on the value space on each block, and you can use negative integer to specify the free slot location and make them into an array-index based doubly linked-list, and you could make the brute forte iteration down to a fast-skipping linked list traversal. However, that technique doesn't address the case where you still need to iterate your char set (2^21 in the unicode scalar case, or 2^8 if you are working on UTF-8) to check every slot and make sure the slots distributions match your need. The technique used in cedar is basically to address this problem, by maintain the bookkeeping of two kind of information: `NInfo` and `Block`

```
struct NInfo {
    sibling: u8, // the index of right sibling, it is 0 if it doesn't have a sibling.
    child: u8,   // the index of the first child
}
```

```
struct Block {
    prev: i32,   // previous block's index, 3 bytes width
    next: i32,   // next block's index, 3 bytes width
    num: i16,    // the number of slots that is free, the range is 0-256
    reject: i16, // a heuristic number to make the search for free space faster, it is the minimum number of iteration in each trie node it has to try before we can conclude that we can reject this block. If the number of kids for the block we are looking for is less than this number then this block is worthy of searching.
    trial: i32,  // the number of times this block has been probed by `find_places` for the free block.
    e_head: i32, // the index of the first empty elemenet in this block
}
```

The `NInfo` is probably standing for "Trie Node Information", it maintains the trie parent-children and siblings information, since these are the information needed when relocating the trie node around. You have to know which node has smaller size in its children, you could just traverse down and counting the number of children one by one. And you could iterate through the sibling chain when you really need to do the move and compare if the slots fit the potential free spaces.

As for `Block`, it maintains the book-keeping of the free space selection, simulating how you look at it of the each block (256 in size) when you squint at the data structure. It book keeps the information like how many free slots this block still have so that you could quickly prune the branch if the number of the free slots is less than the number of children the trie node you are relocating. Further more, the algorithm categorize the blocks into three kinds

* Full: The block where all of the slots are taken
* Closed: The block where all of the slots are taken except for one
* Open: The rest, but most of the time it is the free slots that just allocated at the end of the `base` and `check` when you resize the array.

Each of them is put in the doubly-linked-list of their own kind. During the search process, you only need to look for the free space from `Closed` type block if you are inserting a single-child node, since it only needs one slot. You only need to look for `Open` block when you are relocating a node with more children. And since it is linked list all you need to do is just insert and remove the block from the linked-list, which is constant time. And insert / remove from the right kind of linked list after you update the node with inserted `label`.

## Benchmarks

With the above algorithm mentioned, let's take a look of `cedarwood`'s benchmark, the rust implementation of `cedar`. My laptop's spec is as follows:
```
MacBook Pro (13-inch, 2017, Two Thunderbolt 3 ports)
2.5 GHz Intel Core i7
16 GB 2133 MHz LPDDR3
```

And here is the benchmark against C++ version and `rust-darts`


### Build
impl                time        version
------------------  --------    ---------
cedar                71 ms      2014-06-24
cedarwood            64 ms      0.4.1
rust-darts          201 ms      b1a6813

### Query
impl                time        version
------------------  --------    ---------
cedar                10 ms      2014-06-24
cedarwood            10 ms      0.4.1
rust-darts           18 ms      b1a6813

For the `rust-darts` it is significantly slower than both of `cedar` and `cedarwood`, and it has extra requirement on the construction of the dictionary to be that the dictionary has to be lexicographically sorted. For `cedar` building contains the time for file reading (with 65536 buffer size) so it actually even slightly faster, but the querying time contains memory-only operation. I am too lazy to change the benchmarking code since it is using no-copying techniques by moving the pointer until the next new line and it is troublesome for me to change the rust implementation to be the same way so that to compare apple to apple. The `cedar` code was compiled with `-O3` and rust's code is compiled with `--release` and only measure the time for the operations in the memory. You can see that the build time, that is `update` are roughly the same for C++ and Rust. And in the query case the Rust version is slightly slower than C++ but basically comparable. I couldn't find where I could further optimize the Rust version since the code for the lookup case is quite simple and not much space to optimize on the source code level without going to the `unsafe` resort. Not sure where I could tune to speed it up, please let me know if you identify the part I could do further.  

## Lesson Learned
Rust's implementation is as comparable as C++'s implementation without much effort, I don't need to go with `unsafe` and keep the majority of the code compiler-checked for memory safety. There is only one place where I need to go by `unsafe` block due to linked-list update but not due to the performance. There are a few handy features in C++ that I would miss, like const generics to make the data structure parameterized on the type level, but overall it is not a big issue in the cedar case. Or template specialization where you could leverage on different implementations based on the type you specify. Since `cedarwood` only save the word id in normal mode. It might require that for the support of reduced-trie feature where the value is stored in place, but right now `cedarwood` only has limited support on that so it is no big issue. Overall it is a smooth experience on porting.

