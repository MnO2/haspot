---
layout: post
title: "logq - Analyzing log files in SQL with command-line toolkit, implemented in Rust"
date: 2019-08-16 23:35 
comments: true
categories: 
---

[logq](https://github.com/MnO2/logq) is my latest project and it has reached a reasonably qualitative milestone for me to comfortably introduce it and share about its technical detail.

Let’s start by an example. Imagine that you are in the middle of troubleshooting a production incident and you are suspecting a certain endpoint from the web-server is having problem and result into high latency. Since it is application level and it is not provided by the AWS CloudWatch. It is also the first time that it happened so there isn't any in-house datadog or application level instarumentation setup. And it occurs to you that the access log contains the relevant information and it would be possible for you to calculate the metrics from the log. You download the log from the archive storage and piece together an ad-hoc script in 30 minutes and run the metrics against the log, and the script is implemented in python it gets to pretty slow if the log size is large. Wouldn't it be great if there were command line where you could handle these kind of ad-hoc analysis situation easily? Where no extra dependency setup like ELK or library is needed. That is the motivation to drive to the development of [logq](https://github.com/MnO2/logq), where you could answer the question of "What are the 95th latencies with 5 seconds time frame against application handlers by ignoring the second path segments" easily.

```
> logq query 'select time_bucket("5 seconds", timestamp) as t, url_path_bucket(request, 1, "_") as r, percentile_disc(0.95) within group (order by sent_bytes asc) as bps from elb group by t, r' data/AWSLogs.log
+----------------------------+----------------------------------------------------------------------+----------+
| 2019-06-07 18:45:30 +00:00 | /img/_/300/2r0/54558148eab71c6c2517f1d9.jpg                          | 0.046108 |
+----------------------------+----------------------------------------------------------------------+----------+
| 2019-06-07 18:45:30 +00:00 | /img/_/300/2r0/546db9fd11bcd3c15f0a4ada.jpg                          | 0.073435 |
+----------------------------+----------------------------------------------------------------------+----------+
| 2019-06-07 18:45:30 +00:00 | /img/_/bound/2r0/559153d381974f0e5289a5e4.webm                       | 0.055385 |
+----------------------------+----------------------------------------------------------------------+----------+
| 2019-06-07 18:45:30 +00:00 | /api/_/conversation/5590921f9d37a84e20286e3e                         | 0.04181  |
+----------------------------+----------------------------------------------------------------------+----------+
| 2019-06-07 18:45:35 +00:00 | /img/_/bound/2r0/551c6e78a107308d47055c96.webp                       | 0.063701 |
+----------------------------+----------------------------------------------------------------------+----------+
...
```

[logq](https://github.com/MnO2/logq) inspired by my own need at daily work when troubleshooting production incident. I noticed that there are a few drawbacks by using glueing scripts to analyze the log

1. You spend a lot of time to parse of the log format, but not focus on calculating the metrics helping to troubleshoot your production issues.
2. Most of the log formats are commonly seen and we should ideally abstract it and have every one benefit from the shared abstraction
3. For web-server log cases, the log volume usually is huge, it could be several hundred MB or even a few GB. Doing it in scripting langauges would make yourself impatiently waiting it is running at your local.

For sure you could finely tuned the analytical tooling like AWS Athena or ELK to analyze the very large volume of logs in tens of or hundreds of gigabytes, but often times you just want to ad-hocly analyze logs and don't bother to set things up and cost extra money. Also, the modern laptop/PC is actually powerful enough to analyze gigabytes of log volumes, just that the implementation is not efficient enough for doing that. Implementing [logq](https://github.com/MnO2/logq) in Rust is in hope to resolve those inconvenience and concerns.

To check out more query examples, please check out the [README](https://github.com/MnO2/logq/blob/master/README.md) it has the commonly used queries (at least to the author).

The rest part of the blog post would be focus on the technical detail I faced when I implemented it in Rust.

## Using nom to parse the SQL syntax

When I was evaluating the parsing approach, I was considering among [nom](https://github.com/Geal/nom), 
[combine](https://github.com/Marwes/combine), [pest](https://github.com/pest-parser/pest) and rolling my own lexer / parser. Initially I was  rolling my own lexer / parser since it is the approach what most of the modern industrial strength compilers would do to cope with the incremental parsing and better error message etc. However I found it too laborious to do it at the prototype phase. I turned to look for a parser combinator library instead soon. I wasn't considering [pest](https://github.com/pest-parser/pest) since I am not familiar with PEG and I had the impression where the parser generated from generator are hard to fine tuned for better error message based on my experience of using lex/yacc. Therefore the only options left are [nom](https://github.com/Geal/nom) and [combine](https://github.com/Marwes/combine). I was intimidated by the macro approach adopted by [nom](https://github.com/Geal/nom) (before 5). I read the blog post I could understand it is due to the limitation in the compiler but it's just looks like a horse hard to harness if anything goes wrong due to macro system. I feel a pity not be able to adopt a library where it is fine tuned for performance. Just when I was about to turn to  [combine](https://github.com/Marwes/combine), I found that starting from [nom](https://github.com/Geal/nom) version 5, it is no longer required to use macro approach. The only drawback is that the documentation for nom 5's complete new approach is still lacking, but it looks like I could quickly grasp its concept since I had experience of using [Parsec](http://hackage.haskell.org/package/parsec) in Haskell. And in the end it is the library I decided to stick with. The type like `IResult` and its type parameters are easy to understand, and most of the combinators are battery included in the library, but it still took me some time to figure it out when the thing I would like to do is not included.

### Parsing identifier

It wasn't so clear on how to express the rules of a valid identifier 

1. It consists of underscore, alphabet, digit
2. It should not start with number
3. It shouldn't be all underscore

After checking out the [example](https://github.com/Geal/nom/blob/master/examples/s_expression.rs#L74) provided by the nom repository, I found that you could actually do it in procedural, pretty much you would do if you roll your own parser with `Result` as the returning type. In nom's case you just need to rely on the already provided `ParseError` and `split_at_position1_complete` instead. By providing helping function working on the character level then it is easy to do that.

```
fn identifier<'a, E: nom::error::ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
{
    fn is_alphanumeric_or_underscore(chr: char) -> bool {
        chr.is_alphanumeric() || chr == '_'
    }

    fn start_with_number(s: &str) -> bool {
        if let Some(c) = s.chars().next() {
            "0123456789".contains(c)
        } else {
            false
        }
    }

    fn all_underscore(s: &str) -> bool {
        for c in s.chars() {
            if c != '_' {
                return false;
            }
        }

        true
    }

    let result = input.split_at_position1_complete(
        |item| !is_alphanumeric_or_underscore(item.as_char()),
        nom::error::ErrorKind::Alpha,
    );

    match result {
        Ok((i, o)) => {
            if start_with_number(o) || all_underscore(o) || KEYWORDS.contains(&o) {
                Err(nom::Err::Failure(nom::error::ParseError::from_error_kind(
                    i,
                    nom::error::ErrorKind::Alpha,
                )))
            } else {
                Ok((i, o))
            }
        }
        Err(e) => Err(e),
    }
}
```

### Precedence climbing expression parsing

Traditionally to parse an expression with precedence, you either encode it in the grammar or use bottom-up approach. It is a long resolved problem and the early day approach is Dijkstra's Shunting Yard algorithm. However, most of the manually implemented modern industrial strength compiler use recursively descent approach, which is top down. One approach is by switching to the bottom-up approach when parsing to a point where operator precedence parser is needed. I think it is also possible to do that with parser combinator approach but we need to treat an operator precedence parser as a standalone black box and it doesn't combine elegantly with the rest of the code. And here is the point where I would like to introduce Precedence climbing algorithm. It just combine so elegantly with the parser combinator approach, since it is a top down approach it just looks seamlessly with the rest of the combinators' code. If you are not familiar with Precedence climbing, Eli Bendersky has a very good [introduction](https://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing).

In just a few short lines of code and the whole precedence parsing problem is resolved.
```
fn parse_expression_at_precedence<'a>(
    i0: &'a str,
    current_precedence: u32,
    precedence_table: &HashMap<String, (u32, bool)>,
) -> IResult<&'a str, ast::Expression, VerboseError<&'a str>> {
    let (mut i1, mut expr) = parse_expression_atom(i0)?;
    while let Ok((i2, op)) = parse_expression_op(i1) {
        let (op_precedence, op_left_associative) = *precedence_table.get(op).unwrap();

        if op_precedence < current_precedence {
            break;
        }

        let (i3, b) = if op_left_associative {
            parse_expression_at_precedence(i2, op_precedence + 1, precedence_table)?
        } else {
            parse_expression_at_precedence(i2, op_precedence, precedence_table)?
        };

        let op = ast::BinaryOperator::from_str(op).unwrap();
        expr = ast::Expression::BinaryOperator(op, Box::new(expr), Box::new(b));

        i1 = i3;
    }

    Ok((i1, expr))
}
```

It also worths to mention that I was actually got lazy to encode the precedence in the grammar when I was doing the syntax part in the first iteration and leave the precedence parsing to later to resolve. It is a major refactoring when the project need to shift the approach when I was affirmed that the precedence climbing is the way to go. Without Rust's type system's help, it is a hard take. I only needed to follow the compiler's complaint and clear out all of the warnings/errors then the task is pretty much done. I simply could not imagine how I could do that in a dynamic-typed programming language.

## Failure crate
I would like to briefly talk about the `failure` crate as well. I have had experience of using `failure` crate in some of the mini projects. At the time I was feeling that it is quite convenient that you could derive everything by putting the label on the Error struct. In this larger project, with the help of `impl From` and `#[cause]` label, and `?` operator. I have the strong feeling that it help shape your structuring of error handling into monadic style. `impl From` plays the role kind of like monad-transformer where lift the application from one kind of error monad to another kind, but without having you to stick your head around complicated type combination but it could influence you to put down the error handling in the similar structure.

## Compiling SQL to Graph
Then here comes the core part. Writing a compiler is all about parsing and manipulating on graphs. For translating SQL into computation graph is relatively elegant than translating other procedural programming languages, since if you think it thoroughly, it is translating into the the structure that is pretty functional. 

For a simple select statement, like "select a from foo". It is actually could be translated into a term you are familiar in functional world. That is `map`. Suppose that you `foo` table has its schema defined as ("a", "b", "c"). Then "select a" is just saying, please project to "a" from ("a", "b", "c"), or you could say it a synonym to `map`.

For the `from` clause, it is just saying a data source where you could stream the data record from, it could be a file name or any other data source, either physically or virtually referred to.

`where` clause is a no brainer, it is just interpreting a `filter` against the projected data records from `map`. The expressions to evaluated against are provided to `where` in the form of `a = 1` etc.

The relatively complicated is `group by`, but it is simply just doing a `fold` against a key-value data structure, with the key as the distinct values specified by the fields in the `group by` clause, and the rest of the columns in the values, or sent to the "aggregator". If you are familiar with Haskell you know that HashMap is [foldable](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Foldable.html#t:Foldable), group by is basically a fold against HashMap if it is the backing storage. In other languages like ruby, `reduce` is probably the method used in this kind of action instead.

So to sum up, the whole SQL translation is basically just parse the abstract the syntax tree into a map-reduce chain, streaming the source data from the upstream and produce it to the downstream. in the main stream language this type of structures are called `iterator`, so it could be seen as a series of iterators chained together as well. 

For sure there are details where you would like to rewrite the whole graph to remove the const expression etc, or push down the complicated logic into the data source if the data source supports complicated compute logic (in practice they are smart key-value store supports transaction and cursor). But the spirit of the whole compilation is pretty much as the above mentioned.

## T-Digest
In [logq](https://github.com/MnO2/logq), the aggregate function of `percentile_disc` is supported since it is a common need to calculate the 99th percentile of latency in analyzing web traffic. However, to calculate the exact percentile you have to sort the sequence, which is a very expensive operation if the log file size is huge. `percentile_disc` in logq is compiled to group by with sorting in each of the partition, and it would remember every elements so that it is able to sort. 

In order to make it more efficient in terms of the memory footprint and to support the parallel aggregation feature on the roadmap, I implemented `approx_percentile` aggregate function by T-Digest algorithm. T-Digest is an interesting data sketching algorithm. It leverages on the "scaling function" to make the both ends of the percentiles (for example, 1th percentile or 99th percentile) very accurate and the percentile in the middle less accurate. It matches the use cases we often need. The core concept behind is still by binning the data points, just that it uses scaling function to control the binning process, it makes the binning on both ends more granular so that we could keeps the info as the original as possible and the binning in the middle more blunt (larger binning size). And the choice of the scaling function is pretty flexible, the only requirements are that it has to be non-decreasing, and its derivative starts at 0 and ends at 1. My implementation is based on what facebook's folly provided. In the original paper and many reference implementation it is using `arcsin`, but in folly it is using sqrt since it more computation efficient, and practically not much statistical impact.

I released the T-Digest part as a separate [tdigest](https://crates.io/crates/tdigest) crate. It is not fully tested with generated statistical data, but it should be reasonably correct since I am following folly's implementation.

## Post Words
This is the first non-trivial project implemented in Rust, the line reported by `cloc` on tag `v0.1.4` is around 4800 lines of Rust.

```
github.com/AlDanial/cloc v 1.82  T=0.05 s (305.6 files/s, 120809.5 lines/s)
-------------------------------------------------------------------------------
Language                     files          blank        comment           code
-------------------------------------------------------------------------------
Rust                            13            651             17           4835
YAML                             1              0              0             32
-------------------------------------------------------------------------------
SUM:                            14            651             17           4867
-------------------------------------------------------------------------------
```

To do major refactoring on this size of code usually it is a pain but at least so far I can still feel comfortable to do any major changes without breaking too much things.  

There are plenty of features I plan to put on the roadmap, performance optimization is definitely on the top of the list since I didn't consider too much performance when I coded the proof of concept. There are plenty of room to speed up. Other things on the roadmap are

* Conforms to the much more complicated SQL syntax as sqlite
* Performance optimization, avoid unnecessary parsing
* More supported functions
* time_bucket with arbitrary interval (begin from epoch)
* Window Function
* Implementing approximate_percentile_disc with t-digest algorithm when the input is large.
* Streaming mode to work with tail -f
* Customizable Reader, to follow GoAccess's style
* More supported log format
* Plugin quickjs for user-defined functions

Hope I can constantly have spare time to work on this many things on the roadmap.
