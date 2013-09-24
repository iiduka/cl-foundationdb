;;; -*- Mode: Lisp -*-

(in-package :foundationdb-tests)

(defun run-tests ()
  (api-version 100)
  (unwind-protect
       (progn
         (format t "~&Connecting to cluster...~%")
         (setq *db* (database-open))

         (format t "~&Running tests...~%")
         (basic-tests)
         )
    (database-close *db*)
    (setq *db* nil))
  t)
