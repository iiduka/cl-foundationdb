;;; -*- Mode: Lisp -*-

(defpackage :foundationdb-tests
  (:use #:common-lisp #:foundationdb #:babel
        #+sbcl #:sb-thread)
  (:export #:run-tests))
