---
layout: post
title: "CJK Unified Ideograph developers should know"
date: 2019-06-16 21:51 
comments: true
categories: 
---

I have been working on [jieba-rs](https://github.com/messense/jieba-rs) these few weeks and traced the code from the original python [implementation](https://github.com/fxsjy/jieba). I found a wrong assumption in the code that is by assuming the unicode scalar only ranges from U+4E00 to U+9FD5

```python
re_han_default = re.compile("([\u4E00-\u9FD5a-zA-Z0-9+#&\._%\-]+)", re.U)
```

The same kind of mistakes have been made on Chinese websites, where developers don't even bother to put minimum effort to understand what the unicode standard is, and therefore only assumes that the unicode range only from the BMP. They don't know as of Unicode 12.0, it has defined a total of 87887 CJK Unifided Ideograph. And there are charaters defined in Extension A to Extension F, and the upcoming planned Extension G. What falls in the BMP was mainly the result of [Han Unification](https://en.wikipedia.org/wiki/Han_unification)

To correctly defined the range of the unicode for CJK as the time of Unicode 12, you have to define at least the following ranges.

* U+3400...U+4DBF   (Extesnion A)
* U+4E00...U+9FFF   (BMP)
* U+F900...U+FAFF   (Compatibilty Ideograph)
* U+20000...U+2A6DF (Extension B)
* U+2A700...U+2B73F (Extension C)
* U+2B740...U+2B81F (Extension D)
* U+2B820...U+2CEAF (Extension E)
* U+2CEB0...U+2EBEF (Extension F)
* U+2F800...U+2FA1F (Compatibility Supplement)

It would cover the test cases like so that you would have your logic built on the correct foundation.

```rust
   let mut jieba = Jieba::new();

    //add fake word into dictionary
    jieba.add_word("䶴䶵𦡦", Some(1000), None);
    jieba.add_word("讥䶯䶰䶱䶲䶳", Some(1000), None);

    let words = jieba.cut("讥䶯䶰䶱䶲䶳䶴䶵𦡦", false);
    assert_eq!(words, vec!["讥䶯䶰䶱䶲䶳", "䶴䶵𦡦"]);
```
