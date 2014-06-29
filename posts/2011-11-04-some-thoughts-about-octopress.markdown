---
layout: post
title: "Some thoughts about Octopress"
date: 2011-11-04 04:40
comments: true
categories: 
---

[Octopress](http://octopress.org/) has become a popular blogging framework among programmers since its 2.0 release.
We can observe this growing from the reading of [google keyword tool](http://www.google.com/insights/search/#q=octopress&date=1%2F2011%2012m&cmpt=date).
It did serve the needs of most programmers comparing to other existing blogging frameworks.
To the author, I give my full compliement and respect.

However, as what [luikore](http://luikore.github.com/2011/09/good-things-learned-from-octopress/) said:
> Octopress uses Rdiscount instead of Maruku for Markdown engine. 
> Maruku supports $LATEX$ math but Rdiscount doesn't. 
> Octopress claims itself a blog for hackers, but what hacker doesn't use $LATEX$ math?

Making Octopress to work with MathJax is a pain in the neck (though technically it is not Octopress, it is Jekyll).
Even following the instructions provided by luikore cannot completely resolve the problem. 
I excerpted a few equations from the official site of MathJax to do a display test,
and here it is.

####The Cauchy-Schwarz Inequality
$ \left( \sum_{k=1}^n a_k b_k \right)^2 \leq \left( \sum_{k=1}^n a_k^2 \right) \left( \sum_{k=1}^n b_k^2 \right) $

####A Cross Product Formula
$ \mathbf{V}_1 \times \mathbf{V}_2 =  \begin{vmatrix} \mathbf{i} & \mathbf{j} & \mathbf{k} \\ \frac{\partial X}{\partial u} &  \frac{\partial Y}{\partial u} & 0 \\ \frac{\partial X}{\partial v} &  \frac{\partial Y}{\partial v} & 0
\end{vmatrix} $

####An Identity of Ramanujan
$ \frac{1}{\Bigl(\sqrt{\phi \sqrt{5}}-\phi\Bigr) e^{\frac25 \pi}} = 1+\frac{e^{-2\pi}} {1+\frac{e^{-4\pi}} {1+\frac{e^{-6\pi}} {1+\frac{e^{-8\pi}} {1+\ldots} } } } $

####A Rogers-Ramanujan Identity
$ 1 +  \frac{q^2}{(1-q)}+\frac{q^6}{(1-q)(1-q^2)}+\cdots = \prod_{j=0}^{\infty}\frac{1}{(1-q^{5j+2})(1-q^{5j+3})}, >}uad\quad \text{for $|q|<1$}.>} $

####Maxwell's Equations
$ \begin{aligned} \nabla \times \vec{\mathbf{B}} -\, \frac1c\, \frac{\partial\vec{\mathbf{E}}}{\partial t} & = \frac{4\pi}{c}\vec{\mathbf{j}} \\   \nabla \cdot \vec{\mathbf{E}} & = 4 \pi \rho \\ \nabla \times \vec{\mathbf{E}}\, +\, \frac1c\, \frac{\partial\vec{\mathbf{B}}}{\partial t} & = \vec{\mathbf{0}} \\ \nabla \cdot \vec{\mathbf{B}} & = 0 \end{aligned} $

Some of the equations are still broken after the hack.
I guess it is because `\\` symbols are escaped.
Since I don't have decent knowledge about ruby packages, it may take me some time to fix.
