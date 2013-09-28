;;; -*- Mode: Lisp -*-

(in-package :foundationdb-tests)

(defun counter ()
  (make-counter *db* "counter"))

(defun simple-counter ()
  (let ((counter (counter)))
    (let ((prev (with-transaction (tr *db*)
                  (prog1 (counter-get-transactional counter tr)
                    (counter-add counter tr 1)))))
      (with-transaction (tr *db*)
        (counter-add counter tr 2))
      (assert-equals 'counter-add (+ prev 3)
                     (with-transaction (tr *db*)
                       (counter-get-snapshot counter tr))))))

(defun set-counter ()
  (let ((counter (counter)))
    (with-transaction (tr *db*)
      (counter-set counter tr 100))
    (assert-equals 'counter-clear 100
                   (with-transaction (tr *db*)
                     (counter-get-snapshot counter tr)))))

(defun clear-counter ()
  (let ((counter (counter)))
    (with-transaction (tr *db*)
      (counter-clear counter tr))
    (assert-equals 'counter-clear 0
                   (with-transaction (tr *db*)
                     (counter-get-snapshot counter tr)))))

(defun counter-tests ()
  (clear-counter)
  (simple-counter)
  (set-counter))
