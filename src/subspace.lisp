;;; -*- Mode: Lisp -*-

(in-package :foundationdb)

(defclass subspace ()
  ((raw-prefix :accessor subspace-prefix :initarg :prefix)))

(defun make-subspace (&key prefix raw-prefix)
  (make-instance 'subspace :prefix (if (null prefix)
                                       (if (null raw-prefix)
                                           (make-array 0 :element-type '(unsigned-byte 8))
                                           raw-prefix)
                                       (tuple-encode-item prefix :prefix raw-prefix))))

(defmethod print-object ((obj subspace) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (raw-prefix) obj
      (write raw-prefix :stream stream))))

(defun subspace (subspace name)
  (make-subspace :raw-prefix (subspace-prefix subspace) :prefix name))

(defmethod key-octets ((obj subspace)) (subspace-prefix obj))

(defun subspace-encode-key (subspace item)
  (tuple-encode-item item :prefix (subspace-prefix subspace)))

(defun subspace-decode-key (subspace octets)
  (assert (subspace-contains-key-p subspace octets) (octets)
          "Key ~S is not within ~S" octets subspace)
  (tuple-decode octets :start (length (subspace-prefix subspace))))

(defun subspace-contains-key-p (subspace octets)
  (key-starts-with octets (subspace-prefix subspace)))

(defun key-starts-with (octets prefix)
  (let ((length (length prefix)))
    (and (>= (length octets) length)
         (not (dotimes (i length)
                (unless (= (aref octets i) (aref prefix i))
                  (return t)))))))

(defun subspace-range (subspace &optional item)
  (let ((octets (if (null item)
                    (subspace-prefix subspace)
                    (subspace-encode-key subspace item))))
    ;; Keys strictly within this subspace.
    (make-range (concatenate '(array (unsigned-byte 8) (*)) octets '(#x00))
                (concatenate '(array (unsigned-byte 8) (*)) octets '(#xFF)))))
