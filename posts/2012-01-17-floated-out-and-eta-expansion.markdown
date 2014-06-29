---
layout: post
title: "Floated Out and Eta Expansion"
date: 2012-01-17 06:37
comments: true
categories: haskell 
---

This write-up was inspired by a [question on stackoverflow](http://stackoverflow.com/questions/8779390/what-does-floated-out-mean/8779444#8779444).
The answers explain almost everything, 
but I still want to dig deeper from GHC implementation.
That is to say, I am viewing this problem with ``core langauge`` 
generated with option ``-ddump-simpl`` when you compile haskell programs.

Let's start off with an odd number filter.
{% gist 1633776 filter.hs %}

Without optimization, we get a simple core generated.
(I've removed unrelated I/O parts and cleaned it up.)
{% gist 1633776 filter-O1.core %}
``f1_rbs`` corresponds to ``f1`` and ``f2_rbu`` corresponds to ``f2`` in the source code.
You can see that ``f2_rbu`` has the form of ``\eta_B1 -> (!!) filter... eta_B1``,
but ``f1_rbs`` without a surrounding lambda instead ``(!!) filter...``.
Clearly, ``f2_rbu`` is the [eta expansion](http://www.haskell.org/haskellwiki/Eta_expansion) of ``f1_rbs``.
We also know that the inside of a lamda is evaluated each time we call it.
Therefore, calling ``f2_rbu`` multiple times would results in reconstruction of list.
In ``f1_rbs`` case, the list is shared among different callings.

Next we examine the optimized case with ``-O2`` switched on.
{% gist 1633776 filter-O2.core %}
The optimized code is a little confusing.
The optimized ``f1`` part is definitions from ``Main.main2`` to ``Main.main6``,
and optimized ``f2`` part is from ``Main.main7`` to ``Main.main10``.
Without too much insight you can observe that the content of ``Main.main6`` and ``Main.main10`` are almost the same.
The surrounding lambda of ``f2`` is optimized away.


Now go back to the fibonacci program in the [question](http://stackoverflow.com/questions/8779390/what-does-floated-out-mean/8779444#8779444).
{% gist 1633776 floatout.hs %}

And this is unoptimized version.
{% gist 1633776 floatout-O1.core %}
You can see the pattern that one has lambda outside and another doesn't.
``fib_aoC`` is ``(!!) (map (\ds_ds5 -> ...) enumFrom...)``
and ``fib2_acd`` is ``\x_abA -> (!!) (map (\ds_ds0 -> ...) enumFrom...) x_abA) ``.

As for ``-O2`` version.
We can view the code in two groups: 

* ``Main.main7``, ``Main.main_sgo``, ``go_rUQ``, ``Main.amin6``, ``Main.main5``, which corresponds to ``fib``.
* ``sgo_rUS``, ``go1_rUW``, ``a_rUV``, ``Main.wfib2``, ``Main.main3``, which corresponds to ``fib2``

{% gist 1633776 floatout-O2.core %}

Both definitions are dissected into the form ``sgo`` (indirectly) calls ``go_XXX``, and inside ``go_XXX`` it calls back ``sgo``.
The lambda is optimized away, hence the performance is improved.

The experiment setting is ``GHC 7.0.2`` on ArchLinux, 32 bits.
Here I merely try to view the problem from the core language, and I don't intend to try the affect of type hint as mentioned in the original thread.
