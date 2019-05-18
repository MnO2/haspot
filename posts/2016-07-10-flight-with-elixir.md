---
layout: post
title: "Flight with Elixir"
date: 2016-07-10 16:30
comments: true
categories: 
---
### Flight with Elixir

![](https://cdn-images-1.medium.com/max/2400/1*IZxHEx_bVFJu1F2pQFPZVg.png)

I kept hearing people saying good comments about Elixir and its most popular web
framework: Phoenix, from last year. People pitched the language with the Ruby
syntax however much more performant because it is built on BEAM, leveraging its
robustness and speedy performance. And due to some personal business I had to
travel and took about 5 hours flight and therefore I decided to get my flight
time to have a brief overlook of the language. I am using the “Getting Started”
documentation from the elixir-lang.org. The first impression is unlike other
mainstreet langauge, elixir got a few things right.

1. Reference semantic

The example is extracted from the example of Joe Armstrong’s A week of Elixir.
The creator of the language apparently understand that the mechanics that the
value itself should be immutable and we are just binding a value to a name, if
we would like to “modify” it then we should just rebind the name to a different
value, but not make the semantic having the side-effect instead.

iex(9)> s = HashSet.new<br> #HashSet<[]><br> iex(10)> s = HashSet.put(s,
:element)<br> #HashSet<[:element]>

2. Closure done right

It is common when main street language trying to catch-up with the wagon of
functional programming, they did it wrong. One famous example is python’s
lambda, which obviously didn’t honor the semantic of closure where the value
should be captured and freezed.

>>> a = 1<br> >>> f = lambda x: x + a<br> >>> f(5)<br> 6<br> >>> a = 3<br> >>>
f(5)<br> 8

And Elixir did it right.

iex(11)> a = 5<br> 5<br> iex(12)> f = fn(x) -> x + a end<br>
#Function<6.50752066/1 in :erl_eval.expr/5><br> iex(13)> f.(1)<br> 6<br>
iex(14)> a = 10<br> 10<br> iex(15)> f.(1)<br> 6

3. Macro

I didn’t check the manual to verify if everything in Elixir is an expression or
not, but I played with a few of control flow structure and it seems like so.

iex(1)> if nil do<br> …(1)> “haha”<br> …(1)> else<br> …(1)> “world”<br> …(1)>
end<br> “world”

And the good thing is that I didn’t aware that Elixir actually comes with Macro.
The above if/else control flow is actulally just a macro and rewrite it to the
following structure.

iex(18)> if false, do: :this, else: :that<br> :that

4. Sigil

Not sure if Sigil is also achieved with Macro, but it is great that with a PCRE
like Regex to make the daily scripting handy.

iex(16)> “HELLO” =~ ~r/hello/<br> false

5. Exit is catchable

It should be inherited from BEAM’s feature, most of the languages has exit to
abort the program, but with BEAM’s own lightweight process. you can actually use
exit as a signal to notify the parent process to do something about it.

iex(17)> try do<br> …(17)> exit “lalala”<br> …(17)> catch<br> …(17)> :exit, _ ->
“boom”<br> …(17)> end<br> “boom”

Although I still prefer to program in a well-designed staic type language,
overall I like Elixir. It looks like Ruby on a distant, but actually more about
good inheritance from Erlang under the hood. If its community could catch up the
size of Ruby, then it is definitely better than the world full of Ruby on Rails
shop.

