---
layout: post
title: "Introduction to Template Haskell"
date: 2012-03-05 17:30
comments: true
categories: 
---
This article serves as my digestion of Template Haskell, an GHC extension enables us to use Haskell Language itself to metaprogram. It is extensively used in Yesod Web Framework, so understanding it thoroughly undoubtedly facilitate us run Yesod in its full power. Before we do that, let’s go through common approaches used to metaprogram.

As most of terminology used in the community of programming language, metaprogramming is vaguely defined in my opinion. From the third sence of etymology dictionary, an literal interpretation would be “program of that which trancends the program.” That brings us to the definition from wikipedia:

Metaprogramming is the writing of computer programs that write or manipulate other programs (or themselves) as their data, or that do part of the work at compile time that would otherwise be done at runtime.
For me the important part is to be able to manipulate programs, or code as data. You either do it at runtime, which is usually what most dynamic typing programming language would do, or rely on compilers help you to do so (at compile time).

One of common approaches toward the former one is sacrificing encapsulation to leak the internal of the runtime engine. Popular dynamic typing programming languages like Ruby, Python, PHP, Javascript all choose to do so, but this feature not just involves these languages. Java, instead of stressing on encapsulation, chooses to leak the class information of JVM and achieve the same effect. But this trade-off is benefitial considered that it makes the languages can metaprogram in their own languages, without any extra extension.

Speaking of the latter, we would mention probably the first language what most programmers know, C++ template. Programmers describe a template and instantiate a set of them, then compiler will generate the source code corresponding to the instantiation. C++ template is an outcome of accident. It is discovered to be Turing-complete during the process of standardization, which means you can crash the compiler by writing code never stop. Here is a C++ template metaprogram to generate Fibonacci sequence at compile time.

```
#include <iostream>

template <int N>
struct Fibonacci
{
    enum { value = Fibonacci<N-2>::value + Fibonacci<N-1>::value };
};

template <>
struct Fibonacci<2>
{
    enum { value = 1 };
};

template <>
struct Fibonacci<1>
{
    enum { value = 1 };
};

int main()
{
    int x = Fibonacci<4>::value;
    int y = Fibonacci<20>::value;

    std::cout << x << " " << y << std::endl;
    return 0;
}
```

Lisp family worths another paragraph apart from the previous approaches. They are so special because of homoiconicity: a programming language where the primary representation of programs is also a data-structure in a primitive type of the language itself. In Lisp family, the code itself can be seen as a representation of its abstract syntax tree, with parenthesis as the subtree boundaries. Hence we have such a powerful Macro system in lisp, which directly operates on the abstract syntax tree. Or it can be seen as functions transforming from one syntax tree to another. In clojure, a dialect of lisp run on JVM, we can observe the behavior of macro like this.

First bring up the REPL

```
$ clojure-repl
WARNING: clojure.lang.Repl is deprecated.
Instead, use clojure.main like this:
java -cp clojure.jar clojure.main -i init.clj -r args...
Clojure 1.1.0
user=>
then type

ser=> (defmacro Square [X] `(* ~X ~X))
#'user/Square
user=> (macroexpand-1 '(Square 9))
(clojure.core/* 9 9)
```

Template Haskell, though not homoiconic, imitates Lispy macro behavior. If you are familiar with Common Lisp, Scheme or Clojure, understanding Template Haskell should be fairly easy. It could be vaguely thought as a type-safe, compile time version of lispy macro system.

### Template Haskell
Template Haskell currently is not a part of standard, neither Haskell 98 nor Haskell 2010. It is just an experimental extension of GHC. In order to use it, you have to add a few lines at the head of file. (The environment here is GHC 7.0.4)

```
{-# LANGUAGE TemplateHaskell #-} import Language.Haskell.TH
```

Otherwise in ghci, you have to use operator :set and :module.

```
ghci> :set -XTemplateHaskell 
ghci> :module + Language.Haskell.TH
```

In parallel to Lispy quote, Haskell provides what is called “Quote Monad” type or just “Q Monad”. They works just like Lispy “quote” with four different types for different syntax program structures: expression, declaration, type and pattern To use them, enclose a piece of code in Oxford Brackets [| … |], instead of lispy `(+ ~X ~X).

```
[e| ...(some Haskell code)... |] :: Q Exp   -- Expression 
[d| ...(some Haskell code)... |] :: Q [Dec] -- Declaration
[t| ...(some Haskell code)... |] :: Q Type  -- Type
[p| ...(some Haskell code)... |] :: Q Pat   -- Pattern
```

For those don’t understand Lisp, just think of them as functions mapping a string containing haskell code to its corresponding abstract syntax tree.

In Lisp, to evaluate a macro, just surround it with parenthesis and feed it with correct arguments: (Square 9) In Template Haskell, we use $() to achieve the same effect, and the terminology used is “Splices”.

```
$([d| ... (some haskell code) ... |])
```

And again, you can simply think of it as a function mapping an abstract syntax tree to the corresponding haskell code string.

Let’s start with a very simple example, not hello world, but one plus one. In haskell, it would like this:

With Tempalte Haskell, we can rewrite it like this:

```
{-# LANGUAGE TemplateHaskell #-} 
import Language.Haskell.TH 
f = $([e| 1+1 |]) 
main = print f
```

We replace the 1+1 expression with corresponding Oxford Brackets for expression [e| … |], and generate code (or evalute it) with $(…).

You may also try to include f to our expression.

```
{-# LANGUAGE TemplateHaskell #-} 
import Language.Haskell.TH 
-- error 
$([e| f = 1+1 |]) 
main = print f
```

Sadly it is illegal, because it is a declaration but not an expression. We have to use Oxford Brackets for declaration [d| … |].

```
{-# LANGUAGE TemplateHaskell #-} 
import Language.Haskell.TH 
$([d| f = 1+1 |]) 
main = print f
```

Now you get a little hang of it, we can even include the main declaration.

```
{-# LANGUAGE TemplateHaskell #-} 
import Language.Haskell.TH 
$([d| f = 1+1 |]) 
$([d| main = print f |])
```

Actually, we don’t see a lot of work under water behind this cover of ease of usage. We are in a static typing functional language, so there must be some magical types easy to compose behind the scene. And here we are, let’s type a few characters in ghci.

```
ghci> runQ [e| 1+1 |] InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) (Just (LitE (IntegerL 1)))

```
With runQ, we can acquire the type info behind the scene, they are Q Monads. Watching it closely and you will notice that they corresponds the strcture of abstract syntax tree. (LitE (IntegerL 1)) gives us literal value 1. and InfixE … (VarE GHC.Num.+) … gives us plus operator.

Let’s put it in declaration.

```
ghci> runQ [d| f=1+1 |] 
[ValD (VarP f) (NormalB (InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) (Just (LitE (IntegerL 1))))) []]
```

Now the extra elements are ValD (VarP f) (NormalB …). It is obvious that it is a top-level binding.

You can try it with your own example like hello world:

```
ghci> runQ [d| main=putStrLn "Hello World!" |] 
[ValD (VarP main) (NormalB (AppE (VarE System.IO.putStrLn) (LitE (StringL "Hello World!")))) []]
```

Not every expression or declaration can be such simple, put them in brackets and voila. Sometimes you have to compose them by yourselves, or compiler would complain. Continuing with out hello world example, we can write it like this.

```
main = $(appE (varE $ mkName "putStrLn") (litE (stringL "Hello World!")))
```

Here we compose abstract syntax tree with Q monad, then splice it with $( … ) operator. Or you can use do to glue Q Monad together.

```
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-} 
import Language.Haskell.TH 
do 
  mainQ <- valD (varP $ mkName "main") (normalB (appE (varE 'putStrLn) (litE (stringL "Hello World!")))) [] return [mainQ]
```

### Quasi Quote
Another extension commonly used with Template Haskell is Quasi Quote. It extends the Oxford Brackets beyond the basic four to your likeness, like [foo| … |]

To use that you not only have to switch on the flags,

```
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
ghci> :set -XQuasiQuotes
but also define QuasiQuoter

foo = QuasiQuoter { quoteExp = parseExp :: String -> Q Exp , quotePat = undefined :: String -> Q Pat , quoteType = undefined :: String -> Q Type , quoteDec = undefined :: String -> Q [Dec] }
```

You can see that the four fields correspond to the original four: expression, pattern, type and declaration. It tells quasiquoter to use quoteExp when located at expression context, quotePat when located at pattern context, and so on.

Here is an example from the HaskellWiki. Due to the restriction of Template Haskell, the definition has to go to different source files.

``` 
Main.hs {-# LANGUAGE TemplateHaskell, QuasiQuotes #-} module Main where

import Data.Generics import qualified Language.Haskell.TH as TH import Language.Haskell.TH.Quote import Expr

main :: IO () main = do print $ eval [expr| 1 + 3 + 5 |] case IntExpr 1 of [expr|$int:n|] -> print n _ -> return () ```

``` 

```
{-# LANGUAGE DeriveDataTypeable #-} module Expr (Expr(..), BinOp(..), eval, expr, parseExpr) where

import Data.Generics import Text.ParserCombinators.Parsec import qualified Language.Haskell.TH as TH import Language.Haskell.TH.Quote

data Expr = IntExpr Integer | AntiIntExpr String | BinopExpr BinOp Expr Expr | AntiExpr String deriving(Show, Typeable, Data)

data BinOp = AddOp | SubOp | MulOp | DivOp deriving(Show, Typeable, Data)

eval :: Expr -> Integer eval (IntExpr n) = n eval (BinopExpr op x y) = (opToFun op) (eval x) (eval y) where opToFun AddOp = (+) opToFun SubOp = (-) opToFun MulOp = (*) opToFun DivOp = div

small = lower <|> char ’_‘large = upper idchar = small <|> large <|> digit <|> char’’’

lexeme p = do{ x <- p; spaces; return x } symbol name = lexeme (string name) parens p = between (symbol “(”) (symbol “)”) p

expr’ :: CharParser st Expr expr’ = term chainl1 addop

term :: CharParser st Expr term = factor chainl1 mulop

factor :: CharParser st Expr factor = parens expr’ <|> integer <|> try antiIntExpr <|> antiExpr

mulop = do{ symbol “*“; return $ BinopExpr MulOp } <|> do{ symbol”/“; return $ BinopExpr DivOp }

addop = do{ symbol “+”; return $ BinopExpr AddOp } <|> do{ symbol “-”; return $ BinopExpr SubOp }

integer :: CharParser st Expr integer = lexeme $ do{ ds <- many1 digit ; return $ IntExpr (read ds) }

ident :: CharParser s String ident = do{ c <- small; cs <- many idchar; return (c:cs) }

antiIntExpr = lexeme $ do{ symbol “$int:”; id <- ident; return $ AntiIntExpr id } antiExpr = lexeme $ do{ symbol “$”; id <- ident; return $ AntiExpr id }

parseExpr :: Monad m => (String, Int, Int) -> String -> m Expr parseExpr (file, line, col) s = case runParser p () “” s of Left err -> fail $ show err Right e -> return e where p = do pos <- getPosition setPosition $ (flip setSourceName) file $ (flip setSourceLine) line $ (flip setSourceColumn) col $ pos spaces e <- expr’ eof return e

quoteExprExp :: String -> TH.ExpQ quoteExprPat :: String -> TH.PatQ

expr :: QuasiQuoter expr = QuasiQuoter { quoteExp = quoteExprExp, quotePat = quoteExprPat, quoteType = undefined, quoteDec = undefined }

quoteExprExp s = do loc <- TH.location let pos = (TH.loc_filename loc, fst (TH.loc_start loc), snd (TH.loc_start loc)) expr <- parseExpr pos s dataToExpQ (const Nothing extQ antiExprExp) expr

antiExprExp :: Expr -> Maybe (TH.Q TH.Exp) antiExprExp (AntiIntExpr v) = Just $ TH.appE (TH.conE (TH.mkName “IntExpr”)) (TH.varE (TH.mkName v)) antiExprExp (AntiExpr v) = Just $ TH.varE (TH.mkName v) antiExprExp _ = Nothing

quoteExprPat s = do loc <- TH.location let pos = (TH.loc_filename loc, fst (TH.loc_start loc), snd (TH.loc_start loc)) expr <- parseExpr pos s dataToPatQ (const Nothing extQ antiExprPat) expr

antiExprPat :: Expr -> Maybe (TH.Q TH.Pat) antiExprPat (AntiIntExpr v) = Just $ TH.conP (TH.mkName “IntExpr”) [TH.varP (TH.mkName v)] antiExprPat (AntiExpr v) = Just $ TH.varP (TH.mkName v) antiExprPat _ = Nothing ```
```

Template Haskell can also arouse some problems when working with generic programming. So be careful about using it.
