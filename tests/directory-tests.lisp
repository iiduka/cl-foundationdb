;;; -*- Mode: Lisp -*-

(in-package :foundationdb-tests)

(defvar *root* (make-root-directory))

(defun create-open ()
  (let ((key (subspace-encode-key
              (directory-subspace-open *root* *db*
                                       '("test" "a" "b" "c")
                                       :if-does-not-exist :create)
              "hello")))
    (assert-equals 'create-open
                   key
                   (subspace-encode-key
                    (directory-subspace-open
                     (directory-subspace-open *root* *db* '("test"))
                     *db* '("a" "b" "c"))
                    "hello"))))
                   
(defun directory-tests ()
  (create-open))
