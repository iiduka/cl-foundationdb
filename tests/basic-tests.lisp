;;; -*- Mode: Lisp -*-

(in-package :foundationdb-tests)

(defvar *db*)

(defun assert-equals (test expected actual)
  (assert (equalp expected actual) 
          () "~A failed: expected: ~S; actual: ~S" test expected actual))

(defun byteify (x)
  (typecase x
    (string (babel:string-to-octets x :encoding *foreign-encoding*))
    (list (map 'list #'byteify x))
    (vector (map 'vector #'byteify x))
    (t x)))

(defun background (function)
  #+sbcl (sb-thread:make-thread function :name "Background")
  #+ccl (ccl:process-run-function "Background" function)
  #-(or sbcl ccl) (error "Do not know how to run threads on this system")
  )

(defun set-get ()
  (with-transaction (tr *db*)
    (setf (transaction-get tr "hello") "world"))
  (assert-equals 'set-get
   (byteify "world")
   (with-transaction (tr *db*)
     (future-value (transaction-get tr "hello")))))

(defun clear-get ()
  (with-transaction (tr *db*)
    (transaction-clear tr "hello"))
  (assert-equals 'clear-get
   nil
   (with-transaction (tr *db*)
     (future-value (transaction-get tr "hello")))))

(defun watch-callback ()
  (let ((called-back nil)
        (future (with-transaction (tr *db*)
                  (transaction-watch tr "hello"))))
    (unwind-protect
         (progn
           (setf (future-callback future) #'(lambda (future) 
                                              (declare (ignore future))
                                              (setq called-back t)))
           (background #'(lambda ()
                           (with-transaction (tr *db*)
                             (setf (transaction-get tr "hello") "world"))))
           (sleep .125)
           (assert-equals 'transaction-watch t (future-ready-p future))
           (future-block-until-ready future))
      (future-destroy future))))

(defun key-selectors ()
  (with-transaction (tr *db*)
    (dotimes (i 10)
      (setf (transaction-get tr (format nil "Key ~D" i)) (format nil "~D" i))))
  (with-transaction (tr *db*)
    (assert-equals 'key-selector-last-less-than
     (byteify "Key 0")
     (future-value (transaction-key tr (key-selector-last-less-than "Key 1"))))
    (assert-equals 'key-selector-last-less-or-equal
     (byteify "Key 1")
     (future-value (transaction-key tr (key-selector-last-less-or-equal "Key 1"))))
    (assert-equals 'key-selector-first-greater-or-equal
     (byteify "Key 1")
     (future-value (transaction-key tr (key-selector-first-greater-or-equal "Key 1"))))
    (assert-equals 'key-selector-first-greater-than
     (byteify "Key 2")
     (future-value (transaction-key tr (key-selector-first-greater-than "Key 1"))))))

(defun clear-range ()
  (with-transaction (tr *db*)
    (transaction-clear-range tr "Key 1" "Key 3"))
  (with-transaction (tr *db*)
    (assert-equals 'key-selector-first-greater-or-equal
     (byteify "Key 3")
     (future-value (transaction-key tr (key-selector-first-greater-or-equal "Key 1"))))))

(defun range-query ()
  (assert-equals 'range-query
   (byteify
    '(("Key 0" "0") ("Key 3" "3") ("Key 4" "4") ("Key 5" "5") 
      ("Key 6" "6") ("Key 7" "7") ("Key 8" "8") ("Key 9" "9")))
   (with-transaction (tr *db*)
     (transaction-range-query tr (range-starts-with "Key "))))
  (assert-equals 'range-query
   (byteify
    '(("Key 6" "6") ("Key 5" "5") ("Key 4" "4")))
   (with-transaction (tr *db*)
     (transaction-range-query tr "Key 4" "Key 7" :reverse t)))
  (assert-equals 'range-query
   (byteify
    '(("Key 5" "5") ("Key 6" "6") ("Key 7" "7")))
   (with-transaction (tr *db*)
     (transaction-range-query tr (key-selector-first-greater-than "Key 4") #(#xFF) :limit 3)))
  (assert-equals 'do-range-query
   426
   (with-transaction (tr *db*)
     (let ((total 0))
       (do-range-query ((key value) tr (range-starts-with "Key"))
         (declare (ignore key))
         (incf total (aref value 0)))
       total))))

(defun basic-tests ()
  (set-get)
  (clear-get)
  (watch-callback)
  (key-selectors)
  (clear-range)
  (range-query)
  t)
