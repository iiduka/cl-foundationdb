;;; -*- Mode: Lisp -*-

(in-package :foundationdb-tests)

(defvar *items* (list 1 0 -1 123456789 "hello"
                      (coerce '(#\snowman #\nul) 'string)
                      (coerce '(1 2 3 0 #xFE #xFF) '(array (unsigned-byte 8) (*)))))

(defun encode-decode ()
  (dolist (item *items*)
    (let ((tuple (tuple-decode (tuple-encode item))))
      (assert-equals "Tuple size" 1 (tuple-length tuple))
      (assert-equals (format nil "Encode-decode ~S" item) item (tuple-elt tuple 0)))))

(defun encode-many ()
  (let ((tuple (tuple-decode (apply #'tuple-encode *items*))))
    (assert-equals 'encode-many *items*
                   (loop for i below (tuple-length tuple) 
                         collect (tuple-elt tuple i)))))

(defun build-push ()
  (let ((tuple (make-tuple)))
    (dolist (item *items*)
      (tuple-push tuple item))
    (assert-equals 'build-push (tuple-encode tuple) (tuple-encode *items*))))

(defun subtuple ()
  (let ((tuple (tuple-decode (apply #'tuple-encode *items*))))
    (setq tuple (tuple-subtuple tuple 0 3))
    (assert-equals "Tuple size" 3 (tuple-length tuple))
    (assert-equals "Tuple elements" '(1 0 -1) (loop for i below (tuple-length tuple) 
                                                    collect (tuple-elt tuple i)))))

(defun compare ()
  (let* ((tuple1 (make-tuple 1 2 3))
         (tuple2 (make-tuple 1 2 4))
         (tuple1a (tuple-decode (tuple-encode tuple1))))
    (assert (tuple-lessp tuple1 tuple2) () "~S < ~S" tuple1 tuple2)
    (assert (tuple-equalp tuple1 tuple1a) () "~S = ~S" tuple1 tuple1a)
    (assert (tuple-not-equalp tuple1 tuple2) () "~S /= ~S" tuple1 tuple2)))

(defun tuple-key-value ()
  (with-transaction (tr *db*)
    (setf (transaction-get tr (make-tuple "hello")) (make-tuple "big" "world")))
  (assert-equals 'tuple-key-value
   '("big" "world")
   (with-transaction (tr *db*)
     (let ((tuple (tuple-decode (future-value (transaction-get tr (make-tuple "hello"))))))
       (loop for i below (tuple-length tuple) 
             collect (tuple-elt tuple i))))))

(defun subspace-tests ()
  (let ((subspace (subspace (make-subspace :prefix "stuff") "good")))
    (let ((key (subspace-encode-key subspace "item")))
      (assert-equals 'subspace-contains-key-p t (subspace-contains-key-p subspace key))
      (assert-equals 'subspace-decode-key "item" 
                     (tuple-elt (subspace-decode-key subspace key) 0)))))

(defun tuple-tests ()
  (encode-decode)
  (encode-many)
  (build-push)
  (subtuple)
  (tuple-key-value)
  (subspace-tests))
