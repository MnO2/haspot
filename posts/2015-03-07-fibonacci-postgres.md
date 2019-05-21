---
layout: post
title: "Fibonacci with Postgres"
date: 2015-03-07 17:30
comments: true
categories: 
---
Starting from 9.0, Postgres added Common Table Expression to help you refactor your query from nested query to chaining. It also added recursive keyword to enable a symbol referring to themselves. This creates a loopwhole and make the language Turing Complete. The whole expression makes no difference from a recursive let binding expression.

For example, to recursively compute fibonacci numbers, you could write psql like this.

```
with recursive fib(a, b) as (
        values (0, 1)
    union all
        select greatest(a, b), a+b as a from fib
        where b < 1024
)
select a from fib;
```

Excuting it would create a list of fibonacci numbers.

```
> psql < fib.sql
  a
-----
   0
   1
   1
   2
   3
   5
   8
  13
  21
  34
  55
  89
 144
 233
 377
 610
 987
(17 rows)
```

This is no difference in the form of this recursively bound haskell expression.

```
ghci> let fibs = 0:1:zipWith (+) fibs (tail fibs)
ghci> takeWhile (< 1024) fibs
[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987]>)
```

Explaining the statement and you could see the underlying step by step execution.

```
QUERY PLAN
-----------------------------------------------------------------------------
 CTE Scan on fib  (cost=2.96..3.58 rows=31 width=4)
   CTE fib
     ->  Recursive Union  (cost=0.00..2.96 rows=31 width=8)
           ->  Values Scan on "*VALUES*"  (cost=0.00..0.01 rows=1 width=8)
           ->  WorkTable Scan on fib fib_1  (cost=0.00..0.23 rows=3 width=8)
                 Filter: (b < 1024)
(6 rows)
```

With postgres being Turing complete, the language is basically equivalent to what Prolog does. Just one of the engine stressing more on data management, and the other more on problem solving. As Yin wang said, try challenging yourself by writing a Dijkstra shortest path finding algorithm in psql would be a good practice.
