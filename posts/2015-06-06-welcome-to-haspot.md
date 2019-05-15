---
layout: post
title: "Haspot: Haskell 寫的 static blog generator"
date: 2015-06-06 17:30
comments: true
categories: 
---

[Haspot](https://github.com/MnO2/haspot) 是參考了 Octopress 而製成，基於 Hakyll 的 static blog generator。而命名既然 Octopress 致敬了流行的 Wordpress 服務，那用 Haskell 寫的就致敬一下 Blogspot。就像大多數 Haskell 寫的 package 一樣，開頭都會冠上 ha 。所以就成了 haspot。但一個字一個字翻譯的話，可以看作是 haha + spot，也就是「笑」加上「點」。就成了「笑點」，故得到中文譯名。也就是說，如果你的部落格是用「笑點」生成的話，你就可以名正言順地說：「我的部落格有笑點」，或者是「powered by 笑點」。

### 如何安裝

首先於 [Github repo](https://github.com/MnO2/haspot) checkout 出一份 code，並確保你的系統有安裝 GHC >= 7.8 或者 Haskell Platform 2014。

* 接著創建一份 Sandbox 來 build haspot

``` bash
cabal sandbox init
cabal install
```

* 然後你可以用 build 或 watch 指令來生成 static bllog

``` bash
cabal exec watch
```

* 要新增文章的話，就在 posts 資料夾新增一個前綴日期的 markdwon 就可以了。

