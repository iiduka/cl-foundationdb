;;; -*- Mode: Lisp -*-

(defpackage :foundationdb
  (:use #:common-lisp #:cffi
        #+sbcl :sb-thread)
  (:export #:api-version #:fdb-error))
