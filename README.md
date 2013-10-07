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

Futures
-------

Get operations return futures.

```common-lisp

(with-transaction (tr *db*)
  (dolist (kv '(("a" "alfa") ("b" "bravo") ("c" "charlie") ("d" "delta")))
    (setf (transaction-get tr (first kv)) (second kv))))

(with-transaction (tr *db*)
  (let ((futures (loop for key in '("a" "b" "c" "d")
                       collect (transaction-get tr key))))
    (loop for future in futures
          collect (babel:octets-to-string (future-value future)))))
                       
```

Callbacks
---------

A future can have a callback function which is called when it becomes ready. This callback is called in the FoundationDB client's thread, so it must not do any database operations. Which usually means that it has to interact with some other kind of synchronization mechanism that the application is using.

```common-lisp

(with-transaction (tr *db*)
  (let ((futures (map 'list #'(lambda (key) (transaction-get tr key))
                      '("a" "b" "c" "d")))
        (semaphore (sb-thread:make-semaphore)))
    (flet ((wake-up (future)
             (declare (ignore future))
             (sb-thread:signal-semaphore semaphore)))
      (dolist (future futures)
        (setf (future-callback future) #'wake-up)))
    (loop while futures do
      (sb-thread:wait-on-semaphore semaphore)
      (setq futures (delete-if #'(lambda (future)
                                   (when (future-ready-p future)
                                     (print (babel:octets-to-string (future-value future)))
                                      t))
                               futures)))))

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

Tuples
------

Like mutable vectors with a limited number of element types and encoding to and from 8-bit bytes.

```common-lisp

(let ((t1 (make-tuple 1 2 3 "hello")))
  (tuple-push t1 (coerce '(10 0 #xFE #xFF) '(array (unsigned-byte 8) (*))))
  (let* ((b (tuple-octets t1))
         (t2 (tuple-decode b))
         (t3 (tuple-subtuple t2 1)))
    (print b)
    (print t2)
    (print t3)))

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
