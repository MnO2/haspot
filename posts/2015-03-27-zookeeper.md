---
layout: post
title: "Notes on Apache Zookeeper"
date: 2015-03-27 17:30
comments: true
categories: 
---
As an implementation of Paxos algorithm, Apache zookeeper has been incorporated into different distributed system as a configuration server or synchronization server. Apache Kafka and Solr Cloud are two of prominent open source examples, if you includes inhouse software for service discovery etc there would be more. However, the zookeeper has been known as a headache among Ops people. It is picky about the latency between different instances, and recovery and monitoring are also painful. Netflix released exhibitor ease the pain a little bit, but here I would still jot down the knowledge I am aware so far.

For a five instances zookeeper cluster, the zoo.cfg look like this, you have to specifying the server's ip and port in the configuration

```
server.1=10.0.5.219\:2888\:3888
server.2=10.0.5.219\:2889\:3889
server.3=10.0.5.219\:2890\:3890
server.4=10.0.5.219\:2891\:3891
server.5=10.0.5.219\:2892\:3892
clientPort=2181
dataDir=/var/zookeeper/1
syncLimit=5
tickTime=2000
initLimit=10
dataLogDir=/var/zookeeper/1
```

If something very serious just happened, you have to cleanup the snapshot, you have to clean up the version-2 dir in the server's directory. Then restart the servers. This only applies when you are unable to recover the consistency, using these commands would wipe out everything. Use it carefully.

```
cd /opt/zookeeper/zookeeper-3.4.5/
sudo bin/zkServer.sh stop conf/zoo.cfg
sudo bin/zkServer.sh stop conf/zoo2.cfg
sudo bin/zkServer.sh stop conf/zoo3.cfg
sudo bin/zkServer.sh stop conf/zoo4.cfg
sudo bin/zkServer.sh stop conf/zoo5.cfg

cd /var/zookeeper/1
sudo rm -rf version-2
cd /var/zookeeper/2
sudo rm -rf version-2
cd /var/zookeeper/3
sudo rm -rf version-2
cd /var/zookeeper/4
sudo rm -rf version-2
cd /var/zookeeper/5
sudo rm -rf version-2

sudo bin/zkServer.sh start conf/zoo.cfg
sudo bin/zkServer.sh start conf/zoo2.cfg
sudo bin/zkServer.sh start conf/zoo3.cfg
sudo bin/zkServer.sh start conf/zoo4.cfg
sudo bin/zkServer.sh start conf/zoo5.cfg
```

Zookeeper wouldn't automatically cleanup the accumulated snapshots and log files. You have to manually clean them. -n 10 means only keep the most recent 10 records. In use case like kafka, the space could be consumed very fast, be sure to leave the enough of space for the partition.

```
sudo java -cp zookeeper-3.4.5.jar:lib/log4j-1.2.15.jar:conf:lib/slf4j-api-1.6.1.jar:lib/slf4j-log4j12-1.6.1.jar org.apache.zookeeper.server.PurgeTxnLog /var/zookeeper/1/ /var/zookeeper/1/ -n 10
```
