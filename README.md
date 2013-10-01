FoundationDB
============

A Common Lisp binding for [FoundationDB](http://www.foundationdb.com) key-value store.

Tries to strike a balance between idiomatic Lisp and similarity to other language bindings.

Simple Usage
------------

```common-lisp

(defpackage :fdb-user
  (:use #:common-lisp #:foundationdb))

(in-package :fdb-user)

(api-version 100)

(defvar *db* (database-open))

(with-transaction (tr *db*)
  (setf (transaction-get tr "hello") "world"))

(with-transaction (tr *db*)
  (future-value (transaction-get tr "hello")))

```

Range Queries
-------------

Both DO and MAP style.

```common-lisp

(with-transaction (tr *db*)
  (setf (transaction-get tr "hi") "bye"))

(with-transaction (tr *db*)
  (do-range-query ((key value) tr (range-starts-with "h"))
    (format t "~&~S~T~S~%" key value)))

(with-transaction (tr *db*)
  (map-range-query 'list #'(lambda (key value) 
                             (concatenate 'string (babel:octets-to-string key)
                                           " -- " (babel:octets-to-string value)))
                   tr "a" "z" :reverse-p t))

```

Directory Layer
---------------

```common-lisp

(defvar *root-dir* (make-root-directory))
(defvar *test-dir*)

(setq *test-dir* (directory-subspace-open *root-dir* *db* "test"
                                          :if-does-not-exist :create))

(with-transaction (tr *db*)
  (setf (transaction-get tr (subspace-encode-key *test-dir* "hello"))
        (make-tuple "world")))

(with-transaction (tr *db*)
  (do-range-query ((key value) tr (subspace-range *test-dir*))
    (format t "~&~S~T~S~%" (subspace-decode-key *test-dir* key) (tuple-decode value))))

```
