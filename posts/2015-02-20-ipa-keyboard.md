---
layout: post
title: "IPA Keyboard on Mac OS X"
date: 2015-02-20 17:30
comments: true
categories: 
---
While back in school, we most probably are taught American English with the so called Kenyon and Knott Phonetic Symbol. It is a simplified IPA subset to capture the normal American pronunciation. And if you were taught with British system, then Daniel Jones Phonetic Symbol would be used instead. However, none of these are useful when you expand your eye sight to other languages. Especially when the language you are learning is in a different language family, say you are native in Romance family but learning Sino-Tibetan languages family, or vice versa. You need a phonemic transcription accurately reflect the actual sound. No matter it is tone or aspirated consonant or not.

International Phonetic Alphabet is the de factor standard we use. You could find various IPA courses on youtube. Like 1, 2, 3. You could get a whole picture from watching the video, but you still have to learn it with your ear and muscle.

Besides keep practicing, it is good to type it out when you hear a word. Unfortunately, the default Mac OS X system doesn’t come with an IPA keyboard. If you are a less frequent IPA user, you could use web keyboard, 1 and 2. But for frequent use, installing fonts and keyboard layout are good to have.

With this article as a very good reference: Using IPA fonts with Mac OS X. I installed it successfully on my Mac OS Yosemite.

1. Download the keyboard layout from here:
2. Double click the dmg and copy to your $HOME/Library

```
cp /Volumes/IPA-MACkbd/IPA\ Unicode\ 6.2\(v1.5\)\ MAC.keylayout $HOME/Library/Keyboard\ Layout/
```

1. Download the CharisSIL fonts for IPA
2. Copy the fonts to your Libar

```
cp CharisSIL-5.000/ $HOME/Library/Fonts/
```

1. Launch Settings -> Keyboard -> ‘+’ -> Others, to choose the ‘IPA Unicode 6.2’.
2. Ctrl + Command + Space to switch to IPA input method
And enjoy!
