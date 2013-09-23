FoundationDB
============

A Common Lisp binding for [FoundationDB](http://www.foundationdb.com) key-value store.

Simple Usage
------------

```common-lisp

(defpackage :fdb-user
  (:use #:common-lisp #:foundationdb))

(in-package :fdb-user)

(api-version 100)

(defvar *db* (open-database))

(with-transaction (tr *db*)
  (setf (transaction-get tr "hello") "world"))

(with-transaction (tr *db*)
  (future-value (transaction-get tr "hello")))

```
