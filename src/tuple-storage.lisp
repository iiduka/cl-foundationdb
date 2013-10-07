;;; -*- Mode: Lisp -*-

(in-package :foundationdb)

(defmethod key-octets ((key tuple)) (tuple-octets key))
(defmethod value-octets ((value tuple)) (tuple-octets value))

(defun tuple-range (tuple)
  (let ((octets (tuple-octets tuple)))
    ;; Keys strictly beneath this.
    (make-range (concatenate '(array (unsigned-byte 8) (*)) octets '(#x00))
                (concatenate '(array (unsigned-byte 8) (*)) octets '(#xFF)))))
