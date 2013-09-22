;;; -*- Mode: Lisp -*-

(in-package :foundationdb)

(defparameter *fdb-header-version* 100)

(defun api-version (version)
  (check-error (fdb-select-api-version-impl version *fdb-header-version*)
               "API version"))

(defparameter *db-name* "DB")

(defun open-database (&optional (cluster-file nil) (db-name *db-name*))
  (start-network)
  (cluster-open-database (make-cluster cluster-file) db-name))

(defun make-cluster (&optional (cluster-file nil))
  (with-foreign-object (pcluster 'fdb-cluster)
    (let ((future (fdb-create-cluster (or cluster-file (null-pointer)))))
      (unwind-protect
           (progn
             (check-error (fdb-future-block-until-ready future)
                          "waiting for future")
             (check-error (fdb-future-get-error future)
                          "future result")
             (check-error (fdb-future-get-cluster future pcluster)
                          "cluster result"))
        (fdb-future-destroy future)))
    (make-instance 'cluster :fdb-cluster (mem-ref pcluster 'fdb-cluster))))

(defclass cluster ()
  ((fdb-cluster :reader cluster-fdb-cluster :initarg :fdb-cluster)))

(defun cluster-open-database (cluster &optional (db-name *db-name*))
  (with-foreign-object (pdatabase 'fdb-database)
    (let ((future (with-foreign-string ((str len) db-name)
                    (fdb-cluster-create-database (cluster-fdb-cluster cluster) 
                                                 str (1- len)))))
      (unwind-protect
           (progn
             (check-error (fdb-future-block-until-ready future)
                          "waiting for future")
             (check-error (fdb-future-get-error future)
                          "future result")
             (check-error (fdb-future-get-database future pdatabase)
                          "database result"))
        (fdb-future-destroy future)))
    (make-instance 'database :fdb-database (mem-ref pdatabase 'fdb-database))))

(defclass database ()
  ((fdb-database :reader database-fdb-database :initarg :fdb-database)))

(defvar *network-started* nil)

(defun start-network ()
  (unless *network-started*
    (check-error (fdb-setup-network)
                 "setting up network")
    (start-network-thread)
    (setq *network-started* t)))

(defun stop-network ()
  (when *network-started*
    (check-error (fdb-stop-network)
                 "stopping network")
    (setq *network-started* nil)))
    
(defun start-network-thread ()
  #+sbcl (make-thread #'(lambda () (fdb-run-network)) :name "FDB Network")
  #+ccl (process-run-function "FDB Network" #'(lambda () (fdb-run-network)))
  #-(or sbcl ccl) (error "Do not know how to run threads on this system")
  )

(define-condition fdb-error (error)
  ((code :reader fdb-error-code :initarg :code)
   (message :reader fdb-error-message :initarg :message))
  (:report (lambda (condition stream)
             (format stream "error ~S (~D)" 
                     (fdb-error-message condition)
                     (fdb-error-code condition)))))

(defun check-error (err &optional (format "API error") &rest args)
  (unless (zerop err)
    (error 'fdb-error :code err :message (apply #'format format args))))
