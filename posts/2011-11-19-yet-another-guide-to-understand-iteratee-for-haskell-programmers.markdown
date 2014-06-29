---
layout: post
title: "Yet Another Guide to Understand Iteratee for Haskell Programmers"
date: 2011-11-19 07:23
comments: true
categories: haskell 
---

The concept of iteratee is currently one of topics not much material cover it.
Its rising resulted from its elegancy to overcome the disadvantage of lazy IO,
to which most people blame for the lack of control on resource.
Although it is generally categorized as an advanced topic, 
I don't think it is too difficult to get the hang of it.
It is an advanced topic because in most case lazy IO can just satisfy your needs, 
you only need it when you run into pretty tough/big problems.

I am not going to elaborate the design of iteratee, 
since there are already great introductions on the web.
I will just recommend a reading priority to reduce the extra difficulties ahead.
Here I list three of them from richly stressing in the context to direct implementation

1. [Yesod Book](http://www.yesodweb.com/book/enumerator)
2. [Oleg Kiselyov's talk](http://okmij.org/ftp/Haskell/Iteratee/IterateeIO-talk.pdf)
3. [John Milllikin's tutorial](https://john-millikin.com/articles/understanding-iteratees/)

I believe following this order can ease the loading of your brain.
The introduction provided by Yesod Book adress the problem from the benefit of the inverse of control,
without jamming you too much about the problems of lazy IO.
About lazy IO and the spirit of iteratee, Oleg's slide give an excellent and clear explanation.
It doesn't require you a lot of background knowlege, very easy to read.
And John's tutorial can be directly correspond to concrete package on hackage,
that is [enumerator](http://hackage.haskell.org/package/enumerator-0.4.15).
That would facilitate doing experiment by yourself.

I will illustrate an example about the usage of [iteratee](http://hackage.haskell.org/package/iteratee-0.8.7),
with accompanying testcase demonstrating the problem of lazy IO.
This example is a modification from [Kazu Yamamoto's tutorial](http://www.mew.org/~kazu/proj/enumerator/).
I replace the enumerator package with the iteratee package.
And instead of finding filenames matching given pattern, this example traverse the directories and print the first line of enumerated files (in ByteString.)
The design of the example is intended to exhaust the limited resource, file descriptors, of naive implementation.

## Print all the first lines of files in a directory

Let's look at the naive implementation first.
Since the ByteString is used in the iteratee implementation, to be parallel, 
it imports `Data.ByteString.Lazy.Char8`.
It also uses `readFile` intentionally to be an lazy IO program.
Otherwise not much to address.
{% gist 1378936 allfirstlines_naive.hs %}

Next we look at the iteratee implementation.
I re-implement `enumDir` function from Kazy's tutorial with iteratee package.
It may be a personal taste, an implementation with `enumerator` would be beginner friendly than an implementation with `iteratee` in my opinion.
I also implement an enumeratee to adapt an enumerator of filepaths to an enumerator of bytestrings: `firstLineE`.
In `firstLineE` I adopt enumerator and enumeratee provided by the `iteratee` package to reduce the work.
{% gist 1378936 allfirstlines_iteratee.hs %}

To test the effect of different implementation, I prepare three test cases.
{% gist 1378936 run_test.sh %}
The first test case is the most easy one, a directory containing 500 files,
and the second one is a directory containing more than 2000 files, that would possibly exhaust file descriptors.
The third one add another layer of complication, with directory hierarchy containing a total of more than 2000 files,
The third one is generated with a haskell script as follows:
{% gist 1378936 gen_nuclear_test.hs %}

For convenience, I write a simple Makefile to make the test command be reduced to `make test`, and the clean command to `make clean`
(Note that I use ghc 7.0.2, you have to compile with `-rtsopts` to enable most runtime flags.)
{% gist 1378936 Makefile %}

And my `ulimit -aS` is as follows to see the allowed number of open files for a process.
```
core file size          (blocks, -c) 0
data seg size           (kbytes, -d) unlimited
scheduling priority             (-e) 20
file size               (blocks, -f) unlimited
pending signals                 (-i) 401408
max locked memory       (kbytes, -l) 32
max memory size         (kbytes, -m) unlimited
open files                      (-n) 1024
pipe size            (512 bytes, -p) 8
POSIX message queues     (bytes, -q) 819200
real-time priority              (-r) 0
stack size              (kbytes, -s) 10240
cpu time               (seconds, -t) unlimited
max user processes              (-u) unlimited
virtual memory          (kbytes, -v) unlimited
file locks                      (-x) unlimited
```

The naive result for test case 1.
We can see it is inefficient for the garbage collection time taking 21.3% of time.
```
./allfirstlines_naive test01/ +RTS -sstderr 
      30,731,840 bytes allocated in the heap
       1,160,436 bytes copied during GC
       8,165,024 bytes maximum residency (4 sample(s))
       2,214,040 bytes maximum slop
              12 MB total memory in use (0 MB lost due to fragmentation)

  Generation 0:    51 collections,     0 parallel,  0.00s,  0.00s elapsed
  Generation 1:     4 collections,     0 parallel,  0.00s,  0.00s elapsed

  INIT  time    0.00s  (  0.00s elapsed)
  MUT   time    0.02s  (  0.02s elapsed)
  GC    time    0.01s  (  0.01s elapsed)
  EXIT  time    0.00s  (  0.00s elapsed)
  Total time    0.02s  (  0.02s elapsed)

  %GC time      21.3%  (21.9% elapsed)

  Alloc rate    1,656,435,077 bytes per MUT second

  Productivity  74.1% of total user, 76.2% of total elapsed
```

The iteratee benchmark for test case 1.
The garbage collection time is reduced to 6.6%.
It is still high because we use naive approach to enumerate files in a specific layer of directory.
```
./allfirstlines_iteratee test01/ +RTS -sstderr 
       3,222,812 bytes allocated in the heap
         268,504 bytes copied during GC
         266,296 bytes maximum residency (1 sample(s))
          16,348 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)

  Generation 0:     5 collections,     0 parallel,  0.00s,  0.00s elapsed
  Generation 1:     1 collections,     0 parallel,  0.00s,  0.00s elapsed

  INIT  time    0.00s  (  0.00s elapsed)
  MUT   time    0.01s  (  0.01s elapsed)
  GC    time    0.00s  (  0.00s elapsed)
  EXIT  time    0.00s  (  0.00s elapsed)
  Total time    0.02s  (  0.01s elapsed)

  %GC time       6.6%  (7.0% elapsed)

  Alloc rate    221,758,205 bytes per MUT second

  Productivity  86.3% of total user, 91.9% of total elapsed

```

test case 2 results for naive implementation.
The resource is exhausted, as expected
```
./allfirstlines_naive test02/ +RTS -sstderr 
allfirstlines_naive: test02/1752: openFile: resource exhausted (Too many open files)
      25,549,600 bytes allocated in the heap
       3,387,656 bytes copied during GC
      16,887,092 bytes maximum residency (5 sample(s))
       4,258,052 bytes maximum slop
              22 MB total memory in use (0 MB lost due to fragmentation)

  Generation 0:    40 collections,     0 parallel,  0.00s,  0.00s elapsed
  Generation 1:     5 collections,     0 parallel,  0.01s,  0.01s elapsed

  INIT  time    0.00s  (  0.00s elapsed)
  MUT   time    0.05s  (  0.05s elapsed)
  GC    time    0.01s  (  0.01s elapsed)
  EXIT  time    0.00s  (  0.00s elapsed)
  Total time    0.06s  (  0.06s elapsed)

  %GC time      18.5%  (18.7% elapsed)

  Alloc rate    548,298,210 bytes per MUT second

  Productivity  79.9% of total user, 80.9% of total elapsed

```

test case 2 for iteratee implemetation.
It completes without error, but with a high percentage of GC time.
As previously stated, it is because we use a naive approach to enumerate files at a specific layer.
```
./allfirstlines_iteratee test02/ +RTS -sstderr 
      13,091,456 bytes allocated in the heap
       2,215,796 bytes copied during GC
       1,590,196 bytes maximum residency (2 sample(s))
          38,256 bytes maximum slop
               3 MB total memory in use (0 MB lost due to fragmentation)

  Generation 0:    21 collections,     0 parallel,  0.00s,  0.00s elapsed
  Generation 1:     2 collections,     0 parallel,  0.00s,  0.00s elapsed

  INIT  time    0.00s  (  0.00s elapsed)
  MUT   time    0.06s  (  0.06s elapsed)
  GC    time    0.01s  (  0.01s elapsed)
  EXIT  time    0.00s  (  0.00s elapsed)
  Total time    0.07s  (  0.07s elapsed)

  %GC time       9.2%  (9.3% elapsed)

  Alloc rate    216,670,627 bytes per MUT second

  Productivity  89.0% of total user, 90.3% of total elapsed

```

test case 3 for naive case.
Again, file descriptors are exhausted.
```
./allfirstlines_naive test03/ +RTS -sstderr 
allfirstlines_naive: test03/0/1/1/1/1/1/1/1/1/0/1: openFile: resource exhausted (Too many open files)
      32,198,888 bytes allocated in the heap
       6,282,568 bytes copied during GC
      16,901,268 bytes maximum residency (5 sample(s))
       4,255,424 bytes maximum slop
              22 MB total memory in use (0 MB lost due to fragmentation)

  Generation 0:    50 collections,     0 parallel,  0.01s,  0.01s elapsed
  Generation 1:     5 collections,     0 parallel,  0.01s,  0.01s elapsed

  INIT  time    0.00s  (  0.00s elapsed)
  MUT   time    0.13s  (  0.13s elapsed)
  GC    time    0.02s  (  0.02s elapsed)
  EXIT  time    0.00s  (  0.00s elapsed)
  Total time    0.15s  (  0.15s elapsed)

  %GC time      11.5%  (11.6% elapsed)

  Alloc rate    239,243,962 bytes per MUT second

  Productivity  87.8% of total user, 88.2% of total elapsed

```

test case 3 for iteratee case.
The GC time is greatly reduced since a layer is at most 2 files in it in this test case.
```
./allfirstlines_iteratee test03/ +RTS -sstderr 
      24,048,476 bytes allocated in the heap
          67,792 bytes copied during GC
          22,964 bytes maximum residency (1 sample(s))
          20,068 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)

  Generation 0:    40 collections,     0 parallel,  0.00s,  0.00s elapsed
  Generation 1:     1 collections,     0 parallel,  0.00s,  0.00s elapsed

  INIT  time    0.00s  (  0.00s elapsed)
  MUT   time    0.20s  (  0.20s elapsed)
  GC    time    0.00s  (  0.00s elapsed)
  EXIT  time    0.00s  (  0.00s elapsed)
  Total time    0.20s  (  0.20s elapsed)

  %GC time       0.4%  (0.4% elapsed)

  Alloc rate    122,192,573 bytes per MUT second

  Productivity  99.0% of total user, 99.3% of total elapsed

```

## Footnote
On one hand, this example serves as my excercise to help me understand iteratee,
on the otherhand, it addresses the problem of lazy IO with such a simple concrete case, which is not seen on the web.

I list other useful links for further info.

* [iteratee python yield comparison](http://www.haskell.org/pipermail/haskell-cafe/2011-July/093648.html)
* [oleg's comment on python yield](http://www.haskell.org/pipermail/haskell-cafe/2011-July/094384.html)
* [iteratee links on Haskell Wiki](http://www.haskell.org/haskellwiki/Iteratee_I/O)
