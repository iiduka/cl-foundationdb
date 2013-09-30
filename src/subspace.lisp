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

(defmethod key-bytes ((obj subspace)) (subspace-prefix obj))

(defun subspace-encode-key (subspace item)
  (tuple-encode-item item :prefix (subspace-prefix subspace)))

(defun subspace-decode-key (subspace bytes)
  (assert (subspace-contains-key-p subspace bytes) (bytes)
          "Key ~S is not within ~S" bytes subspace)
  (tuple-decode bytes :start (length (subspace-prefix subspace))))

(defun subspace-contains-key-p (subspace bytes)
  (key-starts-with bytes (subspace-prefix subspace)))

(defun key-starts-with (bytes prefix)
  (let ((length (length prefix)))
    (and (>= (length bytes) length)
         (not (dotimes (i length)
                (unless (= (aref bytes i) (aref prefix i))
                  (return t)))))))

(defun subspace-range (subspace &optional item)
  (let ((bytes (if (null item)
                   (subspace-prefix subspace)
                   (subspace-encode-key subspace item))))
    (make-range (concatenate '(array (unsigned-byte 8) (*)) bytes '(#x00))
                (concatenate '(array (unsigned-byte 8) (*)) bytes '(#xFF)))))
