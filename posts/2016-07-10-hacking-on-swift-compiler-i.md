---
layout: post
title: "Hacking on Swift Compiler (Part 1)"
date: 2016-07-10 17:30
comments: true
categories: 
---
### Hacking on Swift Compiler (Part 1)

![](https://cdn-images-1.medium.com/max/2400/1*5g0SQGJmRTvim0PRhnmvXg.jpeg)

I have been thinking to hack on the industrial-strength compiler for some time.
The last time I worked on a compiler was my third year in the college, where
every student has to take a required compiler course and wrote a C compiler with
lex and yacc. Ever since my graduation I merely work on my own DSL but not a
thorough compiler. With the advance of LLVM and as what Anders Hejlsberg has
described in this Channel 9 video, the architecture of the compiler is no longer
like the old school of a big pipeline, where the compilation would cost you
minutes for a mid size program. The structure of compiler basically would try to
make the compiler frontend as re-usable as possible to the IDE code completion
part and type checking partially on code snippets.

Since around two months ago I started to work on iOS end again. Which got me a
good opportunity to work on Swift compiler which was architected by compiler
expert Chris Lattner. My attempt of hacking on compiler is to make Swift to
compile to golang. My opinion on the golang is that it has a outstanding runtime
however it is so mediocre in the language design. Swift though yet is also not
so advanced but at least striking some balance regarding the ergonomic and the
speed (without resorting to Rust’s linear logic). Also, it got generics! I put
some thought on this and I think it should be feasible since golang as the
target language having garbage collection, most of the semantics could be
one-to-one translated to the language. And I just need to add some keyword to
make it look like libmill:
[https://github.com/sustrik/libmill](https://github.com/sustrik/libmill) , which
provides C lib support for coroutines. And dropping weak, unowned semantic since
golang doesn’t need that.

Swift was designed to have it’s own intermediate langauge, to provide higher
level structure for multi-pass optimization that llvm’s IR cannot provide.

Initially I think I should codegen from SIR but actually the SIR is too low
level for golang. The typechecked swift AST should be enough for that. And
swiftc already providing that. By typing the following in the console

```
    swiftc -dump-ast hello.swift
```

You can get the AST dump in the format like s-expression

```
    (source_file
      (func_decl "main()" type='() -> ()' access=internal
        (parameter_list)
        (brace_stmt
          (pattern_binding_decl
            (pattern_typed type='Int'
              (pattern_named type='Int' 'a')
              (type_ident
                (component id='Int' bind=Swift.(file).Int)))
            (call_expr implicit type='Int' location=hello.swift:2:16 range=[hello.swift:2:16 - line:2:16] nothrow
              (constructor_ref_call_expr implicit type='(_builtinIntegerLiteral: Int2048) -> Int' location=hello.swift:2:16 range=[hello.swift:2:16 - line:2:16] nothrow
                (declref_expr implicit type='Int.Type -> (_builtinIntegerLiteral: Int2048) -> Int' location=hello.swift:2:16 range=[hello.swift:2:16 - line:2:16] decl=Swift.(file).Int.init(_builtinIntegerLiteral:) specialized=no)
                (type_expr implicit type='Int.Type' location=hello.swift:2:16 range=[hello.swift:2:16 - line:2:16] typerepr='Int'))
              (tuple_expr implicit type='(_builtinIntegerLiteral: Int2048)' location=hello.swift:2:16 range=[hello.swift:2:16 - line:2:16] names=_builtinIntegerLiteral
                (integer_literal_expr type='Int2048' location=hello.swift:2:16 range=[hello.swift:2:16 - line:2:16] value=1))))

    (var_decl "a" type='Int' access=private let storage_kind=stored)
    )))
```

If you would like to get the type alone, you can use swift-ide-test for that

```
    swift-ide-test -print-types -source-filename=hello.swift
```

You would get

```
    FuncDecl '''main''' () -> ()
        VarDecl '''a''' Int
        BinaryExpr:2 '''1 + 1''' Int
          DeclRefExpr:2 '''+''' (Int, Int) -> Int
          TupleExpr:2 '''1 + 1''' (Int, Int)
            CallExpr:2 '''1''' Int
              ConstructorRefCallExpr:2 '''1''' (_builtinIntegerLiteral: Int2048) -> Int
                TypeExpr:2 '''1''' Int.Type
                DeclRefExpr:2 '''1''' Int.Type -> (_builtinIntegerLiteral: Int2048) -> Int
              TupleExpr:2 '''1''' (_builtinIntegerLiteral: Int2048)
                IntegerLiteralExpr:2 '''1''' Int2048
            CallExpr:2 '''1''' Int
              ConstructorRefCallExpr:2 '''1''' (_builtinIntegerLiteral: Int2048) -> Int
                TypeExpr:2 '''1''' Int.Type
                DeclRefExpr:2 '''1''' Int.Type -> (_builtinIntegerLiteral: Int2048) -> Int
              TupleExpr:2 '''1''' (_builtinIntegerLiteral: Int2048)
                IntegerLiteralExpr:2 '''1''' Int2048
        VarDecl '''b''' <<error type>>
        BinaryExpr:3 '''a + 2.0''' <<error type>>
          OverloadedDeclRefExpr:3 '''+''' <<error type>>
          TupleExpr:3 '''a + 2.0''' <<error type>>
            DeclRefExpr:3 '''a''' Int
            FloatLiteralExpr:3 '''2.0''' <<error type>>
```

Which should be enough of information for all of the codegen. I just need to
write a parser for the AST dump and walk the tree to generate the corresponding
golang code. This would be left for the future series to detail my attempt.

In the meantime of digging into the code base, I was also interested into how
swift implemented it’s type system and type checker. I presented my learning on
Hindley Milner Type system at Haskell.sg before, so I have a basic understanding
of how the HM-like system works. However, for industrial strength compiler
especially with the bridging to ObjC part, I am really interested into how to
architect your type system and make it work.

The swift repository on github is pretty undocumented, I can only find two docs
drafting the whole picture but without too much detail. You have to peak into
the code to look for detail.

[https://github.com/apple/swift/blob/master/docs/TypeChecker.rst](https://github.com/apple/swift/blob/master/docs/TypeChecker.rst)

After digging into the code base I found that you can actually turn on the debug
mode in Swift REPL when you compile your swift binary with DEBUG flag on. You
just simply use your DEBUG version swift to run into REPL mode and type the
following meta command

```
    (swift) :constraints debug on
```

Then it would spit all of the type checking process step by step

```
    (swift) let c = 1
    ---Initial constraints for the given expression---
    (integer_literal_expr type='$T0' location=<REPL Input>:1:9 range=[<REPL Input>:1:9 - line:1:9] value=1)
    Score: 0 0 0 0 0 0 0 0 0 0 0 0
    Type Variables:
      #0 = $T0
      #1 = $T1

    Active Constraints:

    Inactive Constraints:
      $T0 conforms to IntegerLiteralConvertible [[locator@0x7ffcc5819e00 [IntegerLiteral@<REPL Input>:1:9]]];
      $T0 conv $T1 [[locator@0x7ffcc5819e00 [IntegerLiteral@<REPL Input>:1:9]]];
    Active bindings: $T0 := Int
    (trying $T0 := Int
      Active bindings: $T1 := Int
      (trying $T1 := Int
        (found solution 0 0 0 0 0 0 0 0 0 0 0 0)
      )
    )
    ---Solution---
    Fixed score: 0 0 0 0 0 0 0 0 0 0 0 0
    Type variables:
      $T1 as Int
      $T0 as Int

    Overload choices:
```

It even come with the Constraint Graph with the calculated connected components
for debugging the solver. Pretty neat.

```
    ---Constraint graph---
      $T0:

    $T1:

    $T2:
        Constraints:
          $T2 conforms to FloatLiteralConvertible [[locator@0x7ffcc4967838 [FloatLiteral@<REPL Input>:1:13]]];
          $T2 operator arg conv Int [[locator@0x7ffcc4967ca8 [Binary@<REPL Input>:1:11 -> apply argument -> comparing call argument #1 to parameter #1]]];

    $T3:
        Constraints:
          $T3 conv $T4 [[locator@0x7ffcc4967b80 [Binary@<REPL Input>:1:11]]];
        Adjacencies: $T4

    $T4:
        Constraints:
          $T3 conv $T4 [[locator@0x7ffcc4967b80 [Binary@<REPL Input>:1:11]]];
        Adjacencies: $T3
```

The above is the progess so far. I also found that Swift compiler is actually a
good source of C++ code base for reading. It adopts pretty modernized C++
syntax, just it is a little pity that the compiler is not reaching to
bootstrapping where the compiler is written in its own language.

