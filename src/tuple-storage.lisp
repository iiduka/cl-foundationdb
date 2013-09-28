;;; -*- Mode: Lisp -*-

(in-package :foundationdb)

(defmethod key-bytes ((key tuple)) (tuple-bytes key))
(defmethod value-bytes ((value tuple)) (tuple-bytes value))
