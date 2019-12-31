---
layout: post
title: "Linux materials I read in the past few weeks"
date: 2019-12-29 22:14 
comments: true
categories: 
---

I've been watching videos and books to get myself familiar with eBPF tooling, more toward Linux network stack and VFS. This post serves as a note on what I've learned.

<iframe width="560" height="315" src="https://www.youtube.com/embed/bj3qdEDbCD4" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

Brendan Gregg has listed the command line tools that are useful for analyzing a Linux instance if he has to ssh into the instance. They could be preliminary analysis to quickly get a feel on what's going on on a specific instance.

1. uptime
2. dmesg -T | tail
3. vmstat 1
4. mpstat -P ALL 1
5. pidstat 1
6. iostat -xz 1
7. free -m
8. sar -n DEV 1
9. sar -n TCP,ETCP 1
10. top

To dig deeper into the detail, these eBPF-based tooling would be helpful.

1. execsnoop
2. opensnoop
3. ext4slower
4. biolatency
5. biosnoop
6. cachestat
7. tcpconnect
8. tcpaccept
9. tcpretrans
10. gethostlatency
11. runqlat
12. profile

When the information you are trying to get are not there, and `strace` is not enough. The following tools could be helpful to peek into the critical paths in the kernel modules.

1. trace
2. argdist

A nice book to read is a [new book](https://www.amazon.com/Performance-Tools-Addison-Wesley-Professional-Computing-ebook-dp-B081ZDXNL3/dp/B081ZDXNL3) authored by Brendan.

While studying for the SRE materials, I also found Cloudflare has many good blog posts that I could learn from.

A blog post on how to optimize for [http2](https://blog.cloudflare.com/http-2-prioritization-with-nginx/) stack gives really good insight on how the Linux network stack works.

```
net.core.default_qdisc = fq
net.ipv4.tcp_congestion_control = bbr
net.ipv4.tcp_notsent_lowat = 16384
```

Their interview questions also makes you start to question yourself if you really know the modern TCP/IP stack.

```
Archaeology

    What is the lowest TCP port number?

    The TCP frame has an URG pointer field, when is it used?

    Can the RST packet have a payload?

    When is the "flow" field in IPv6 used?

    What does the IP_FREEBIND socket option do?

Forgotten Quirks

    What does the PSH flag actually do?

    The TCP timestamp is implicated in SYN cookies. How?

    Can a "UDP" packet have a checksum field set to zero?

    How does TCP simultaneous open work? Does it actually work?

Fragmentation and Congestion

    What is a stupid window syndrome?

    What are the CWE and ECE flags in TCP header?

    What is the IP ID field and what does it have to do with DF bit? Why do some packets have a non-zero IP ID and a DF set?

Fresh Ideas

    Can a SYN packet have a payload? (hint: new RFC proposals)

    Can a SYN+ACK packet have a payload?

ICMP Path MTU

    ICMP packet-too-big messages are returned by routers and contain a part of the original packet in the payload. What is the minimal length of this payload that is accepted by Linux?

    When an ICMP packet-too-big message is returned by an intermediate router it will have the source IP of that router. In practice though, we often see a source IP of the ICMP message to be identical to the destination IP of the original packet. Why could that happen?

Linux Configuration

    Linux has a "tcp_no_metrics_save" sysctl setting. What does it save and for how long?

    Linux uses two queues to handle incoming TCP connections: the SYN queue and the accept queue. What is the length of the SYN queue?

    What happens if the SYN queue grows too large and overflows?

Touching the router

    What are BGP bogons, and why are they less of a problem now?

    TCP has an extension which adds MD5 checksums to packets. When is it useful?

And finally:

    What are the differences in checksumming algorithms in IPv4 and IPv6?
```



