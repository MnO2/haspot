---
layout: post
title: "Tuning Windows 10 for Productivity"
date: 2018-12-29 17:30
comments: true
categories: 
---
As a developer I used OS X for my daily work. Apart from that, with the risk of
being tagged as non-hardcore developers by using Windows, I do used Windows 10 a
lot for my own private setup. It’s easier to setup the hardware you prefer
including graphic cards within a much affordable budget by choosing a PC, and
even though I am a big supporter of free software. It’s also painful to worry
about Linux desktop environment stopped working due to a recent upgrade. With
the introduction of the Linux subsystem on Windows 10 last year, another barrier
was removed. It has become more stable now and I would like to put down the note
on how to make Windows 10 more pleasant to use to serve as a memo.

The game changing component on Windows 10 is definitely the Windows Subsystem on
Linux. I installed beta and took a brief look at the time when it was released.
There was a lot of minor issues back then, but after a year of releases. It is
now more stable and you have various choices on Linux. You could install not
only the latest distribution of Ubuntu, you could also use Kali Linux and others
as well. I could easily move my command line experience to Windows easily by
importing the dotfiles I maintained. The only thing that missing is Docker,
after all it is just a subsystem and the underlying is still Windows Kernel. You
could rely on Docker on Windows but that requires Professional version and
Hypervisor and I don’t want to pay for that. Installing a Linux server on
Virtualbox is more cost effective just for development.

![](https://cdn-images-1.medium.com/max/2600/1*OHQiPTLPKvZCyjRvCLEO3w.png)

For a long period of time, the downside of Windows system is the lack of package
managers, where you could skip the process of going to the website and download
the corresponding installers. With Chocolatey you could install all those
programs as you do on Linux distributions now. Hit the Powershell in
Administrator mode and type choco install with package name, everything would be
done in a minute.

![](https://cdn-images-1.medium.com/max/2600/1*NvweODiISDEHfQyWF4j7SA.png)

Windows key could replace what you have on OS X, with Window + s, you could just
launch the search bar and search for anything you would like, pretty much close
to Spotlight you have on OS X. Also you could drag the programs you commonly
used to the bottom bar, with Window + <num> then you could launch the program
without your hands leaving the keyboard.

![](https://cdn-images-1.medium.com/max/2400/1*sF5hdXyor8Bb2JzKdFL6PQ.png)

A few of long wanted hotkeys was also added: like Window + up to maximize the
window, Window + left or right to align the focus window to half of the screen.
Window + tab to the task view, and Window + Ctrl + D to create a virtual
desktop, and Window + Ctrl + left or right to switch between virtual desktops.

I often also choose Create Shortcut from Chrome to create the desktop icon to
website, so that I could open the website simply by clicking Window + <num> key.
As you saw I put youtube and facebook in the bottom bar.

With the native support of SAMBA it is great that I could simply type \\NAS to
open the NAS server talking in SAMBA in the same LAN. On Linux you probably need
to install extra packages to enable it. Instead of it just comes for as default.

Windows as a popular OS, it comes with various vulnerability. I downloaded
Windows [hardentools ](https://github.com/securitywithoutborders/hardentools)to
disable certain features. It is easily reverted the change. It also disabled the
command line execution so it might be inconvenient if you need to run command
line, but you could simply revert the change when you need it for infrequent
users.

Other than the OS itself, just want to mention that I found that [chrome
bookmark
search](https://chrome.google.com/webstore/detail/bookmark-search/hhmokalkpaiacdofbcddkogifepbaijk?hl=en)
is a useful tool. You could simply press Alt + D when you are using Chrome and
it would jump to the Omnibar, then you could just type bm then press tab you
could search the bookmarks by pattern matching keywords. It works like Alfred in
Chrome. Not Windows specific but just want to mention it since useful.

That’s all the tips.
