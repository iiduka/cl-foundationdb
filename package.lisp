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
   #:future-callback
   #:future-cancel
   #:future-destroy
   #:future-ready-p
   #:future-value
   #:key-bytes
   #:key-selector
   #:key-selector-last-less-than
   #:key-selector-last-less-or-equal
   #:key-selector-first-greater-than
   #:key-selector-first-greater-or-equal
   #:make-cluster
   #:network-set-option
   #:network-start
   #:network-stop
   #:open-database
   #:transaction-addresses-for-key
   #:transaction-cancel
   #:transaction-clear
   #:transaction-commit
   #:transaction-commit-or-retry
   #:transaction-committed-version
   #:transaction-destroy
   #:transaction-get
   #:transaction-key
   #:transaction-range
   #:transaction-read-version
   #:transaction-reset
   #:transaction-set
   #:transaction-set-option
   #:transaction-snapshot
   #:transaction-watch
   #:value-bytes
   #:with-transaction
))
