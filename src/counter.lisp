;;; -*- Mode: Lisp -*-

(in-package :foundationdb)

;;; Pretty straight port from Java, which was a pretty straight port from Python.

(defparameter *random-key-size* 20)
(defparameter *coalesce-limit* 20)
(defparameter *coalesce-probability* 0.1)

(defclass counter ()
  ((db :accessor counter-database :initarg :database)
   (key-prefix :accessor counter-key-prefix :initarg :prefix)
   (random-state :initform (make-random-state t))
   (range)))

(defmethod initialize-instance :after ((obj counter) &key)
  (with-slots (key-prefix range) obj
    (setf range (range-starts-with key-prefix))))

(defun make-counter (db &rest items)
  (make-instance 'counter :database db
                          :prefix (apply #'tuple-encode items)))

(defmethod print-object ((obj counter) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-slots (key-prefix) obj
      (write key-prefix :stream stream))))

(defun counter-get-transactional (counter tr)
  (compute-sum counter tr))

(defun counter-get-snapshot (counter tr)
  (compute-sum counter (transaction-snapshot tr)))

(defun counter-add (counter tr x)
  (add-with-coalesce counter tr x t))

(defun counter-set (counter tr x)
  (let ((value (counter-get-snapshot counter tr)))
    (counter-add counter tr (- x value))))

(defun counter-clear (counter tr)
  (with-slots (range) counter
    (transaction-clear-range tr (range-begin-key range) (range-end-key range))))

(defun add-with-coalesce (counter tr x maybe-coalesce)
  (transaction-set tr (encode-new-key counter) (encode-value x))
  (with-slots (random-state) counter
    (when (and maybe-coalesce (< (random 1.0 random-state) *coalesce-probability*))
      (coalesce counter *coalesce-limit*))))

(defun compute-sum (counter tr)
  (let ((total 0))
    (with-slots (range) counter
      (do-range-query ((key value) tr range)
        (declare (ignore key))
        (incf total (decode-value value))))
    total))

(defun coalesce (counter limit)
  (handler-case
      (with-slots (db range random-state) counter
        (with-transaction (tr db :retry nil)
          (let ((bound (encode-new-key counter))
                (total 0))
            (flet ((coalesce-key (key value)
                     (incf total (decode-value value))
                     (transaction-get tr key)
                     (transaction-clear tr key)))
              (if (zerop (random 2 random-state))
                  (map-range-query nil #'coalesce-key tr
                                   bound (range-end-key range)
                                   :limit limit)
                  (map-range-query nil #'coalesce-key tr
                                   (range-begin-key range) bound
                                   :limit limit :reverse-p t)))
            (add-with-coalesce counter tr total nil))))
    (error (err)
     ;; Log someplace?
     (format *error-output* "Coalescing failure: ~S" err))))

(defun encode-new-key (counter)
  (with-slots (random-state key-prefix) counter
    (let ((rand-key (make-array *random-key-size* :element-type '(unsigned-byte 8))))
      (dotimes (i (length rand-key))
        (setf (aref rand-key i) (random 256 random-state)))
      (tuple-encode-item rand-key :prefix key-prefix))))

(defun encode-value (x)
  (tuple-encode x))

(defun decode-value (value)
  (tuple-elt (tuple-decode value) 0))
