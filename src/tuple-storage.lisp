;;; -*- Mode: Lisp -*-

(in-package :foundationdb)

(defmethod key-bytes ((key tuple)) (tuple-bytes key))
(defmethod value-bytes ((value tuple)) (tuple-bytes value))

(defun tuple-range (tuple)
  (let ((bytes (tuple-bytes tuple)))
    (make-range (concatenate '(array (unsigned-byte 8) (*)) bytes '(#x00))
                (concatenate '(array (unsigned-byte 8) (*)) bytes '(#xFF)))))
