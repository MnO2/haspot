---
layout: post
title: "elasticsearch_dsl: Domain Specific Language in Python for Elasticsearch"
date: 2015-05-30 17:30
comments: true
categories: 
---
An emerging trend in recent year for software, including mongodb, elasticsearch and Chef, is to expose an JSON interface to accept complex requests. They give up the traditional SQL query and adopt JSON as the text encoding of abstract syntax tree. Therefore, whenever you are making up a request to these services, you are actually hand coding an abstract syntax tree in JSON. Although it is flexible and easy to extend, it is also error prone and hard to maintain. A common solution for this is to write a Domain Specific Language. And with python's language design, a naive and natural solution is to use Class to denote AST node and Visitor pattern to code-generate the underlying JSON. And above is the reason I created elasticsearch_dsl

Think that you type the program this way:

```
nested_agg = ast.NestedAggregation("name", "tags", ast.TermsAggregation("tags.name", size=20, order_type="_count", order="desc", min_doc_count=100))
aggregation = ast.TopLevelAggregation("tag", nested_agg)
ast_root = ast.TopLevelQuery(MatchAllQuery(), aggs=[aggregation])

codegen = CodeGeneratorVisitor()
codegen.visit(ast_root)
query_py_obj = codegen.query
and_clauses = []
and_clauses.append(ast.GeoDistanceFilter("geocode", user.geocode["latitude"], user.geocode["longitude"], 10))

tag_names = []
should_clauses= []
for t, ind in tags:
    should_clauses.append(ast.TermQuery("tags.name", t.name.lower()))

nested_queries= ast.BoolQuery(should=should_clauses)
f = ast.NestedQuery("tags", nested_queries)

query = ast.FilteredQuery(f, ast.AndFilter(and_clauses))
query_size = 20
query_from = 0
ast_root = ast.TopLevelQuery(query, query_size, query_from, sort={"_score": {"order": "desc"}})

codegen = CodeGeneratorVisitor()
codegen.visit(ast_root)
query_py_obj = codegen.query
```

But not this way

```
query = {}
query["tags"] = []
for t, ind in tags:
    query["tags].append({"tags.name": t.name.lower()})

query["size"] = query_size
query["from"] = query_from
query["sort"] = {"_score": {"order": "desc"}}
```

which is error prone, and hard to know the intention of each query.
There has been a elasticsearch-dsl package on pypi, but one thing I don't like is the big runtime that comes with it. If anything gets wrong, you have to read almost all of its implementation to know what is error and how to fix it. And this version of elasticsearch_dsl is just a thin wrapper to compile the class into JSON. You could even do it in the command line.
