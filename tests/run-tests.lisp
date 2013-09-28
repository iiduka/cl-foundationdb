;;; -*- Mode: Lisp -*-

(in-package :foundationdb-tests)

(defun run-tests (&optional (cluster-file nil))
  (api-version 100)
  (unwind-protect
       (progn
         (format t "~&Connecting to cluster...~%")
         (setq *db* (database-open cluster-file))

         (format t "~&Running tests...~%")
         (basic-tests)
         (tuple-tests)
         (counter-tests))
    (database-close *db*)
    (setq *db* nil))
  t)
