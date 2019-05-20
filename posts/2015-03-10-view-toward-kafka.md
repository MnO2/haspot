---
layout: post
title: "My view toward Apache Kafka"
date: 2015-05-10 17:30
comments: true
categories: 
---

Apache Kafka is a distributed, fault tolerent high throughput transaction log system. Most of the marketing materials coin it as a message broker or publish-subscribe system. I couldn't say those claims are wrong because in most of the architectures it does serve as a centralized logging system, and pass the message between different parts of the architectural diagram. However, I like to say it as a transaction log system because its API is relatively low-level in semantic comparing to RabbitMQ and others systems that would first come up in your mind when mentioned for the (task) message queue. The offset of the consumers are managed by the consuemr themselves (though starting from 0.8, the offsets of simple consumers are stored in Zookeeper for simple use cases.) This distiction might be because just like any data store abstraction are leaky, the message queue system when put in different scenario, are also leaky. And Apache Kafka lies on the end of low-level API with high throuhput end of the spectrum.

Apache Kafka is carefully well-designed and there have been several articles from its creator and Confluent, the company backing the Apache Kafka.

The intrinsic difficulty of distributed systems also applied to Kafka. As Kyle Kingsbury pointed out. The claims that Kafka picks CA out of CAP theorem doesn't make much sense, because the undefinedness during partition would eventually hurts the availability. Kafka is more like a CP system under certain constraints. And the singular point failure mode was indicated in the article.

From my experience with Kafka, I would say it is quite stable to use, as long as you deploy it with the correct settings and architecture. And you python binding is good enough for minor stream processing cases. You don't have to go for complex stream processing framework such as Apache Samza and Twitter's Storm. If you are familiar with Haskell's lazy evaluation. Any of the Kafka's topic is just like a variable binding with a lazy stream behind. The variable is bound to a stream. And what you have to do is just use a pure function to transform the stream into another binding. I could denote them in let topic-2 = f . g $ topic-1. And with a few of sinking streams consuming the data into database or search engine's index. This programming model ease the pain of data store replication, since the "thing" behind the variable are not in the same memory address space, and the cost of copy is huge.

Kafka saves a lot of its states (consumer group offset, replica leader, partition infos) and configuration in Zookeeper. Therefore you must make sure Zookeeper doesn't go wrong. And you could infer from that most of the operational task is to set a variable in Zookeeper. Like to reset an offset of a given consumer group and partition, it is setting a variable in Zookeeper directory. And to let it consume from start, you just delete that variable remember the offset of that consumer group.

The following is my notes to common operational tasks.

### create topic
```
./bin/kafka-topics.sh --create --zookeeper localhost:2181 --replication-factor 1 --partitions 4 --topic hello-world
```

### tail messages in a topic

```
bin/kafka-console-consumer.sh --zookeeper localhost:2181 --topic hello-world
```

### cat messages in a topic

```
bin/kafka-console-consumer.sh --zookeeper localhost:2181 --topic hello-world --from-beginning
```

### write to a topic

```
bin/kafka-console-producer.sh --zookeeper localhost:2181 --topic hello-world
```

### get the offset

```
bin/kafka-run-class.sh kafka.tools.GetOffsetShell --topic=hello-world --server=kafka://127.0.0.1:8001 --time=-1
```

### dump segment files

```
bin/kafka-run-class.sh kafka.tools.DumpLogSegments kafka-logs/hello-world-0/00000000000034305339.kafka
```

### get the offset before a certain time

```
bin/kafka-run-class.sh kafka.tools.GetOffsetShell --topic=hello-world --server=kafka://127.0.0.1:8001 --time=1406877452000 --offsets=1
```

### print message greater than an offset

```
bin/kafka-run-class.sh kafka.tools.SimpleConsumerShell --server=kafka://127.0.0.1:8001 --topic=hello-world --offset=34305339 --fetchsize=5000 --print-message
```
