;;; -*- Mode: Lisp -*-

(defpackage :foundationdb
  (:use #:common-lisp #:cffi
        #+sbcl :sb-thread)
  (:export 
   #:*foreign-encoding*
   #:api-version
   #:cluster-destroy
   #:cluster-open-database
   #:cluster-set-option
   #:database-create-transaction
   #:database-destroy
   #:database-set-option
   #:fdb-error
   #:future-block-until-ready
   #:future-value
   #:key-bytes
   #:make-cluster
   #:network-set-option
   #:network-start
   #:network-stop
   #:open-database
   #:transaction-cancel
   #:transaction-clear
   #:transaction-commit
   #:transaction-commit-or-retry
   #:transaction-committed-version
   #:transaction-destroy
   #:transaction-get
   #:transaction-range
   #:transaction-read-version
   #:transaction-reset
   #:transaction-set
   #:transaction-set-option
   #:transaction-snapshot
   #:value-bytes
   #:with-transaction
))
