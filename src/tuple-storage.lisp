;;; -*- Mode: Lisp -*-

(in-package :foundationdb)

(defmethod key-bytes ((key tuple)) (tuple-bytes key))
(defmethod value-bytes ((value tuple)) (tuple-bytes value))

(defun tuple-range (tuple &key prefix)
  (range-starts-with (tuple-encode-item tuple :prefix prefix)))
