;;; -*- Mode: Lisp -*-

(in-package :foundationdb)

(defparameter *fdb-header-version* 100)
(defvar *version-set* nil)

(defun api-version (version)
  (cond ((eql *version-set* version) nil)
        (*version-set*
         (error "API version already set to ~D" *version-set*))
        (t
         (check-error (fdb-select-api-version-impl version *fdb-header-version*)
                      "API version")
         (setq *version-set* version)
         t)))

(define-condition fdb-error (error)
  ((code :reader fdb-error-code :initarg :code)
   (message :reader fdb-error-message :initarg :message))
  (:report (lambda (condition stream)
             (format stream "error ~S: ~S"
                     (fdb-error-message condition)
                     (fdb-get-error (fdb-error-code condition))))))

(defun check-error (err &optional (format "API error") &rest args)
  (unless (zerop err)
    (error 'fdb-error :code err :message (apply #'format nil format args))))

;; network-stop is final; it cannot ever be restarted in same process.
(defvar *network-started* nil)

(defun network-start ()
  (unless *network-started*
    (check-error (fdb-setup-network)
                 "setting up network")
    (network-start-thread)
    (setq *network-started* t)))

(defun network-stop ()
  (when *network-started*
    (check-error (fdb-stop-network)
                 "stopping network")
    (setq *network-started* nil)))
    
(defun network-start-thread ()
  #+sbcl (sb-thread:make-thread #'(lambda () (fdb-run-network)) :name "FDB Network")
  #+ccl (ccl:process-run-function "FDB Network" #'(lambda () (fdb-run-network)))
  #-(or sbcl ccl) (error "Do not know how to run threads on this system")
  )

(defun foreign-bytes-to-lisp (length value)
  (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length)
      (setf (aref bytes i) (mem-aref value :uint8 i)))
    bytes))

(defvar *foreign-encoding* :utf-8)

(defmacro with-foreign-option ((value-pointer value-length) value &body body)
  `(multiple-value-bind (,value-pointer ,value-length)
    (foreign-option-alloc ,value)
    (unwind-protect
         (progn . ,body)
      (foreign-free ,value-pointer))))

(defun foreign-option-alloc (value)
  (etypecase value
    (integer
     (values (foreign-alloc :int64 :initial-element value) 8))
    (string
     (foreign-string-alloc value :encoding *foreign-encoding* :null-terminated-p nil))
    ((simple-array (unsigned-byte 8) (*))
     (values (foreign-alloc :uint8 :initial-contents value) (length value)))))

(defun network-set-option (option value)
  (with-foreign-option (value-pointer value-length) value
    (check-error (fdb-network-set-option option value-pointer value-length)
                 "setting option")))

(defparameter *db-name* "DB")

(defun database-open (&optional (cluster-file nil) (db-name *db-name*))
  (network-start)
  (cluster-open-database (make-cluster cluster-file) db-name))

(defun database-close (database)
  (database-destroy database)
  (cluster-destroy (database-cluster database)))

(defclass cluster ()
  ((fdb-cluster :reader cluster-fdb-cluster :initarg :fdb-cluster)))

(defun make-cluster (&optional (cluster-file nil))
  (with-foreign-object (pcluster 'fdb-cluster)
    (let ((future (fdb-create-cluster (typecase cluster-file
                                        (null (null-pointer))
                                        (pathname (namestring cluster-file))
                                        (t cluster-file)))))
      (unwind-protect
           (progn
             (check-error (fdb-future-block-until-ready future)
                          "waiting for future")
             (check-error (fdb-future-get-error future)
                          "creating cluster")
             (check-error (fdb-future-get-cluster future pcluster)
                          "cluster result"))
        (fdb-future-destroy future)))
    (make-instance 'cluster :fdb-cluster (mem-ref pcluster 'fdb-cluster))))

(defun cluster-destroy (cluster)
  (fdb-cluster-destroy (cluster-fdb-cluster cluster)))

(defun cluster-set-option (cluster option value)
  (with-foreign-option (value-pointer value-length) value
    (check-error (fdb-cluster-set-option (cluster-fdb-cluster cluster)
                                         option value-pointer value-length)
                 "setting option")))

(defclass database ()
  ((cluster :reader database-cluster :initarg :cluster)
   (fdb-database :reader database-fdb-database :initarg :fdb-database)))

(defun cluster-open-database (cluster &optional (db-name *db-name*))
  (with-foreign-object (pdatabase 'fdb-database)
    (let ((future (with-foreign-string ((str len) db-name 
                                        :encoding *foreign-encoding*
                                        :null-terminated-p nil)
                    (fdb-cluster-create-database (cluster-fdb-cluster cluster)
                                                 str len))))
      (unwind-protect
           (progn
             (check-error (fdb-future-block-until-ready future)
                          "waiting for future")
             (check-error (fdb-future-get-error future)
                          "opening database")
             (check-error (fdb-future-get-database future pdatabase)
                          "database result"))
        (fdb-future-destroy future)))
    (make-instance 'database :cluster cluster
                             :fdb-database (mem-ref pdatabase 'fdb-database))))

(defun database-destroy (database)
  (fdb-database-destroy (database-fdb-database database)))

(defun database-set-option (database option value)
  (with-foreign-option (value-pointer value-length) value
    (check-error (fdb-database-set-option (database-fdb-database database)
                                          option value-pointer value-length)
                 "setting option")))

(defclass transaction ()
  ((fdb-transaction :reader transaction-fdb-transaction :initarg :fdb-transaction)
   (snapshot :reader transaction-snapshot-p :initarg :snapshot :initform nil)))

;;; TODO: Is is actually useful to have the same transaction in both
;;; snapthot and regular modes?
(defun transaction-snapshot (transaction)
  (make-instance 'transaction
                 :fdb-transaction (transaction-fdb-transaction transaction)
                 :snapshot t))

(defun database-create-transaction (database &key snapshot)
  (with-foreign-object (ptransaction 'fdb-transaction)
    (check-error (fdb-database-create-transaction (database-fdb-database database) ptransaction)
                 "creating transaction")
    (make-instance 'transaction 
                   :fdb-transaction (mem-ref ptransaction 'fdb-transaction)
                   :snapshot snapshot)))

(defun transaction-destroy (transaction)
  (fdb-transaction-destroy (transaction-fdb-transaction transaction)))

(defun transaction-cancel (transaction)
  (fdb-transaction-cancel (transaction-fdb-transaction transaction)))

(defun transaction-set-option (transaction option value)
  (with-foreign-option (value-pointer value-length) value
    (check-error (fdb-transaction-set-option (transaction-fdb-transaction transaction)
                                             option value-pointer value-length)
                 "setting option")))

(defun transaction-read-version (transaction)
  (with-foreign-object (pversion :int64)
    (let ((future (fdb-transaction-get-read-version (transaction-fdb-transaction transaction))))
      (unwind-protect
           (progn
             (check-error (fdb-future-block-until-ready future)
                          "waiting for future")
             (check-error (fdb-future-get-error future)
                          "read version")
             (check-error (fdb-future-get-version future pversion)
                          "database result"))
        (fdb-future-destroy future)))
    (mem-ref pversion :int64)))

(defun transaction-set-read-version (transaction version)
  (fdb-transaction-set-read-version (transaction-fdb-transaction transaction) version))

(defsetf transaction-read-version transaction-set-read-version)
      
(defun transaction-commit (transaction)
  (let ((future (fdb-transaction-commit (transaction-fdb-transaction transaction))))
    (unwind-protect
         (progn
           (check-error (fdb-future-block-until-ready future)
                        "waiting for future")
           (check-error (fdb-future-get-error future)
                        "committing transaction"))
      (fdb-future-destroy future))))

(defun transaction-commit-or-retry (transaction)
  (let ((err
         (let ((future (fdb-transaction-commit (transaction-fdb-transaction transaction))))
           (unwind-protect
                (progn
                  (check-error (fdb-future-block-until-ready future)
                               "waiting for future")
                  (fdb-future-get-error future))
             (fdb-future-destroy future)))))
    (unless (zerop err)
      (let ((future (fdb-transaction-on-error (transaction-fdb-transaction transaction) err)))
        (unwind-protect
             (progn
               (check-error (fdb-future-block-until-ready future)
                            "waiting for future")
               (check-error (fdb-future-get-error future)
                            "committing transaction")
               t)                       ; Reset for retry.
          (fdb-future-destroy future))))))

(defun transaction-committed-version (transaction)
  (with-foreign-object (pversion :int64)
    (check-error (fdb-transaction-get-committed-version (transaction-fdb-transaction transaction) pversion)
                 "getting committed version")
    (mem-ref pversion :int64)))

(defun transaction-reset (transaction)
  (fdb-transaction-reset (transaction-fdb-transaction transaction)))

(defun with-transaction-internal (body database &key (retry t) (snapshot nil))
  (when (typep database 'transaction)   ; TODO: Define transactionp?
    (return-from with-transaction-internal
      (if (and snapshot (not (transaction-snapshot-p database)))
          (transaction-snapshot database)
          database)))
  (let ((transaction (database-create-transaction database :snapshot snapshot)))
    (unwind-protect
         (loop
           (block next
             (return
               (multiple-value-prog1
                   (funcall body transaction)
                 (if retry
                     (when (transaction-commit-or-retry transaction)
                       (return-from next))
                     (transaction-commit transaction))))))
      (transaction-destroy transaction))))

(defmacro with-transaction ((transaction database &rest args) &body body)
  `(with-transaction-internal #'(lambda (,transaction) . ,body) ,database . ,args))
      
(defclass future ()
  ((fdb-future :accessor future-fdb-future :initarg :fdb-future)
   (callback :accessor future-callback-internal :initform nil)
   (callback-id :accessor future-callback-id :initform nil)
   (error :accessor future-error :initform nil)))

(defun future-cancel (future)
  (fdb-future-cancel (future-fdb-future future)))

(defun future-block-until-ready (future)
  (when (future-error future)
    (error (prog1 (future-error future) (setf (future-error future) nil))))
  ;; TODO: Is there some way that plays nicer with Lisp than blocking
  ;; in C, like waiting on a semaphore set by a callback?
  (unless (fdb-future-is-ready (future-fdb-future future))
    (check-error (fdb-future-block-until-ready (future-fdb-future future)) 
                 "waiting for future")
    (check-error (fdb-future-get-error (future-fdb-future future)) 
                 "future result")))

(defun future-ready-p (future)
  (fdb-future-is-ready (future-fdb-future future)))

(defvar *callback-id-counter* 0)
(defvar *callback-registry* (make-hash-table))

#+sbcl
(defvar *callback-registry-mutex* (sb-thread:make-mutex :name "FoundationDB callback registry"))
#+ccl
(defvar *callback-registry-lock* (ccl:make-lock "FoundationDB callback registry"))

(defmacro with-callback-registry-lock (&body body)
  #+sbcl
  `(sb-thread:with-mutex (*callback-registry-mutex*)
    . ,body)
  #+ccl
  `(ccl:with-lock-grabbed (*callback-registry-lock*)
    . ,body)
  #-(or sbcl ccl)
  `(progn . ,body))

(defun callback-register (callback)
  (with-callback-registry-lock
    (let ((id (incf *callback-id-counter*)))
      (setf (gethash id *callback-registry*) callback)
      id)))

(defun callback-deregister (id)
  (with-callback-registry-lock
    (remhash id *callback-registry*)))

(defcallback future-callback :void ((fdb-future fdb-future) 
                                    (callback-parameter :pointer))
  (let ((future (with-callback-registry-lock
                  (gethash (pointer-address callback-parameter) *callback-registry*))))
    (unless (null future)
      (assert (pointer-eq fdb-future (future-fdb-future future)))
      (handler-case
          (funcall (future-callback-internal future) future)
        (error (error) (setf (future-error future) error))))))
               
(defun future-callback (future)
  (future-callback-internal future))

(defun future-set-callback (future callback)
  (setf (future-callback-internal future) callback)
  (when (null (future-callback-id future))
    (let ((id (callback-register future)))
      (setf (future-callback-id future) id)
      (fdb-future-set-callback (future-fdb-future future) 
                               (get-callback 'future-callback)
                               (make-pointer id)))))

(defsetf future-callback future-set-callback)

(defun future-destroy (future)
  (when (future-fdb-future future)
    (fdb-future-destroy (future-fdb-future future))
    (setf (future-callback-internal future) nil)
    (when (future-callback-id future)
      (callback-deregister (future-callback-id future))
      (setf (future-callback-id future) nil))
    (setf (future-fdb-future future) nil)))

(defgeneric future-ready-value (future)
  (:documentation "Called when future is ready to get its value"))

(defun future-value (future)
  (unwind-protect
       (progn
         (future-block-until-ready future)
         (future-ready-value future))
    (future-destroy future)))

(defclass get-future (future) 
  ())

(defmethod future-ready-value ((future get-future))
  (with-foreign-objects ((ppresent 'fdb-bool-t)
                         (pvalue '(:pointer :uint8))
                         (plength :int))
    (check-error (fdb-future-get-value (future-fdb-future future)
                                       ppresent pvalue plength)
                 "getting value")
    (when (mem-ref ppresent 'fdb-bool-t)
      (prog1
          (foreign-bytes-to-lisp (mem-ref plength :int) (mem-ref pvalue :pointer))
        (fdb-future-release-memory (future-fdb-future future))))))

(defgeneric key-bytes (key)
  (:documentation "Get a byte array for key")
  (:method ((key sequence)) key)
  (:method ((key string)) (babel:string-to-octets key :encoding *foreign-encoding*)))

(defmacro with-foreign-bytes ((pointer length) (bytes-fun value) &body body)
  (let ((bytes-var (gensym "BYTES"))
        (i-var (gensym "I")))
    `(let ((,bytes-var (,bytes-fun ,value)))
      (with-foreign-pointer (,pointer (length ,bytes-var) ,length)
        (dotimes (,i-var ,length)
          (setf (mem-aref ,pointer :uint8 ,i-var) (aref ,bytes-var ,i-var)))
        . ,body))))

(defun transaction-get (transaction key)
  (let ((fdb-future (with-foreign-bytes (key-name key-name-length) (key-bytes key)
                      (fdb-transaction-get (transaction-fdb-transaction transaction)
                                           key-name key-name-length
                                           (transaction-snapshot-p transaction)))))
    (make-instance 'get-future :fdb-future fdb-future)))

(defgeneric value-bytes (value)
  (:documentation "Get a byte array for value")
  (:method ((value sequence)) value)
  (:method ((value string)) (babel:string-to-octets value :encoding *foreign-encoding*)))

(defun transaction-set (transaction key value)
  (with-foreign-bytes (key-name key-name-length) (key-bytes key)
    (with-foreign-bytes (value-name value-name-length) (value-bytes value)
      (fdb-transaction-set (transaction-fdb-transaction transaction)
                           key-name key-name-length
                           value-name value-name-length))))

(defsetf transaction-get transaction-set)

(defun transaction-clear (transaction key)
  (with-foreign-bytes (key-name key-name-length) (key-bytes key)
    (fdb-transaction-clear (transaction-fdb-transaction transaction)
                           key-name key-name-length)))

(defun transaction-atomic-operate (transaction key param operation)
  (with-foreign-bytes (key-name key-name-length) (key-bytes key)
    (with-foreign-bytes (param-name param-name-length) (value-bytes param)
      (fdb-transaction-atomic-op (transaction-fdb-transaction transaction)
                                  key-name key-name-length
                                  param-name param-name-length
                                  operation))))

(defun transaction-clear-range (transaction begin-key end-key)
  (with-foreign-bytes (begin-key-name begin-key-name-length) (key-bytes begin-key)
    (with-foreign-bytes (end-key-name end-key-name-length) (key-bytes end-key)
      (fdb-transaction-clear-range (transaction-fdb-transaction transaction)
                                   begin-key-name begin-key-name-length
                                   end-key-name end-key-name-length))))

(defun transaction-add-conflict-range (transaction begin-key end-key
                                       &optional (type :write))
  (with-foreign-bytes (begin-key-name begin-key-name-length) (key-bytes begin-key)
    (with-foreign-bytes (end-key-name end-key-name-length) (key-bytes end-key)
      (fdb-transaction-add-conflict-range (transaction-fdb-transaction transaction)
                                          begin-key-name begin-key-name-length
                                          end-key-name end-key-name-length
                                          type))))

(defclass no-value-future (future) 
  ())

(defmethod future-ready-value ((future no-value-future)) t)

(defun transaction-watch (transaction key)
  (let ((fdb-future (with-foreign-bytes (key-name key-name-length) (key-bytes key)
                      (fdb-transaction-watch (transaction-fdb-transaction transaction)
                                             key-name key-name-length))))
    (make-instance 'no-value-future :fdb-future fdb-future)))

(defclass string-array-future (future) 
  ())

(defmethod future-ready-value ((future string-array-future))
  (with-foreign-objects ((pstrings '(:pointer (:pointer :string)))
                         (pcount :int))
    (check-error (fdb-future-get-string-array (future-fdb-future future) pstrings pcount)
                 "getting value")
    (let* ((count (mem-ref pcount :int))
           (array (mem-ref pstrings :pointer))
           (strings (make-array count :element-type 'string :initial-element "")))
      (dotimes (i count)
        (setf (aref strings i) (foreign-string-to-lisp (mem-aref array :pointer i)
                                                       :encoding *foreign-encoding*)))
      (fdb-future-release-memory (future-fdb-future future))
      strings)))

(defun transaction-addresses-for-key (transaction key)
  (let ((fdb-future (with-foreign-bytes (key-name key-name-length) (key-bytes key)
                      (fdb-transaction-get-addresses-for-key (transaction-fdb-transaction transaction)
                                                             key-name key-name-length))))
    (make-instance 'string-array-future :fdb-future fdb-future)))
  
(defclass key-selector ()
  ((key :reader key-selector-key :initarg :key)
   (or-equal :reader key-selector-or-equal  :initarg :or-equal)
   (offset :reader key-selector-offset  :initarg :offset)))

(defun key-selector-last-less-than (key)
  (make-instance 'key-selector :key (key-bytes key) :or-equal nil :offset 0))

(defun key-selector-last-less-or-equal (key)
  (make-instance 'key-selector :key (key-bytes key) :or-equal t :offset 0))

(defun key-selector-first-greater-than (key)
  (make-instance 'key-selector :key (key-bytes key) :or-equal t :offset +1))

(defun key-selector-first-greater-or-equal (key)
  (make-instance 'key-selector :key (key-bytes key) :or-equal nil :offset +1))

(defun key-selector-add (key-selector offset)
  (make-instance 'key-selector :key (key-selector-key key-selector) :or-equal (key-selector-or-equal key-selector) :offset (+ (key-selector-offset key-selector) offset)))

(defmacro with-foreign-key-selector ((name name-length or-equal offset) key-selector &body body)
  (let ((ks-var (gensym "KS")))
    `(let ((,ks-var ,key-selector))
       (with-foreign-bytes (,name ,name-length) (key-bytes (key-selector-key ,ks-var))
         (let ((,or-equal (key-selector-or-equal ,ks-var))
               (,offset (key-selector-offset ,ks-var)))
           . ,body)))))

(defclass key-future (future) 
  ())

(defmethod future-ready-value ((future key-future))
  (with-foreign-objects ((pvalue '(:pointer :uint8))
                         (plength :int))
    (check-error (fdb-future-get-key (future-fdb-future future)
                                     pvalue plength)
                 "getting value")
    (prog1
        (foreign-bytes-to-lisp (mem-ref plength :int) (mem-ref pvalue :pointer))
      (fdb-future-release-memory (future-fdb-future future)))))

(defun transaction-key (transaction key-selector)
  (let ((fdb-future (with-foreign-key-selector (key-name key-name-length or-equal offset) key-selector
                       (fdb-transaction-get-key (transaction-fdb-transaction transaction)
                                                key-name key-name-length
                                                or-equal offset
                                                (transaction-snapshot-p transaction)))))
    (make-instance 'key-future :fdb-future fdb-future)))

(defclass range ()
  ((begin-key :reader range-begin-key :initarg :begin-key)
   (end-key :reader range-end-key :initarg :end-key)))

(defun make-range (begin-key end-key)
  (make-instance 'range :begin-key begin-key :end-key end-key))

(defun key-successor (bytes)
  (let ((length (length bytes)))
    (loop while (and (plusp length) (= (aref bytes (1- length)) #xFF)) do
      (decf length))
    (when (zerop length)
      (error "There is no successor to ~S" bytes))
    (let ((copy (subseq bytes 0 length)))
      (incf (aref copy (1- length)))
      copy)))

(defun range-starts-with (prefix)
  (let* ((begin (key-bytes prefix))
         (end (key-successor begin)))
    (make-range begin end)))

(defgeneric range-key-selectors (begin-key-or-range end-key))
(defgeneric range-begin-key-selector (key-or-key-selector))
(defgeneric range-end-key-selector (key-or-key-selector))

(defmethod range-key-selectors (begin end)
  (values (range-begin-key-selector begin)
          (range-end-key-selector end)))
  
(defmethod range-key-selectors ((range range) (end null))
  (values (range-begin-key-selector (range-begin-key range))
          (range-end-key-selector (range-end-key range))))

(defmethod range-begin-key-selector (key) (key-selector-first-greater-or-equal key))
(defmethod range-end-key-selector (key) (key-selector-first-greater-or-equal key))

(defmethod range-begin-key-selector ((key-selector key-selector)) key-selector)
(defmethod range-end-key-selector ((key-selector key-selector)) key-selector)

(defclass range-query ()
  ((begin-key-selector :accessor range-query-begin-key-selector :initarg :begin-key-selector)
   (end-key-selector :accessor range-query-end-key-selector :initarg :end-key-selector)
   (limit :accessor range-query-limit :initarg :limit :initform nil)
   (target-bytes :reader range-query-target-bytes :initarg :target-bytez :initform nil)
   (mode :reader range-query-mode :initarg :mode :initform :iterator)
   (iteration :accessor range-query-iteration :initform 0)
   (reverse-p :reader range-query-reverse-p :initarg :reverse :initform nil)
   (more-p :accessor range-query-more-p :initform t)))

(defun make-range-query (begin &optional end &rest args)
  (multiple-value-bind (begin-key-selector end-key-selector)
      (range-key-selectors begin end)
    (apply #'make-instance 'range-query :begin-key-selector begin-key-selector :end-key-selector end-key-selector args)))

(defclass range-future (future)
  ((range-query :reader range-future-range-query :initarg :range-query)))

(defmethod future-ready-value ((future range-future))
  (range-future-ready-map-values 'list #'list future))

(defun range-future-ready-map-values (result-type function future)
  (with-foreign-objects ((pkv '(:pointer (:struct fdb-key-value)))
                         (pcount :int)
                         (pmore 'fdb-bool-t))
    (check-error (fdb-future-get-keyvalue-array (future-fdb-future future)
                                                pkv pcount pmore)
                 "getting key-values")
    (let* ((kv (mem-ref pkv :pointer))
           (count (mem-ref pcount :int))
           (result (if result-type (make-sequence result-type count)))
           key value)
      (dotimes (i count)
        (setq key (foreign-bytes-to-lisp
                   (foreign-slot-value kv '(:struct fdb-key-value) 'key-length)
                   (foreign-slot-value kv '(:struct fdb-key-value) 'key))
              value (foreign-bytes-to-lisp 
                     (foreign-slot-value kv '(:struct fdb-key-value) 'value-length)
                     (foreign-slot-value kv '(:struct fdb-key-value) 'value)))
        (let ((elem (funcall function key value)))
          (when result
            (setf (elt result i) elem)))
        (incf-pointer kv (foreign-type-size '(:struct fdb-key-value))))
      (let ((range-query (range-future-range-query future))
            (more-p (mem-ref pmore 'fdb-bool-t)))
        (when (range-query-limit range-query)
          (decf (range-query-limit range-query) count)
          (when (<= (range-query-limit range-query) 0)
            (setq more-p nil)))
        (setf (range-query-more-p range-query) more-p)
        (when (and more-p (not (null key)))
          (if (range-query-reverse-p range-query)
              (setf (range-query-end-key-selector range-query)
                    (key-selector-first-greater-or-equal key))
              (setf (range-query-begin-key-selector range-query)
                    (key-selector-first-greater-than key)))))
      (fdb-future-release-memory (future-fdb-future future))
      result)))

(defun transaction-range-query-next (transaction range-query)
  (when (range-query-more-p range-query)
    (let ((fdb-future 
           (with-foreign-key-selector (begin-key-name begin-key-name-length begin-or-equal begin-offset) (range-query-begin-key-selector range-query)
             (with-foreign-key-selector (end-key-name end-key-name-length end-or-equal end-offset) (range-query-end-key-selector range-query)
               (fdb-transaction-get-range (transaction-fdb-transaction transaction)
                                          begin-key-name begin-key-name-length
                                          begin-or-equal begin-offset
                                          end-key-name end-key-name-length
                                          end-or-equal end-offset
                                          (or (range-query-limit range-query) 0)
                                          (or (range-query-target-bytes range-query) 0)
                                          (range-query-mode range-query)
                                          (incf (range-query-iteration range-query))
                                          (transaction-snapshot-p transaction)
                                          (range-query-reverse-p range-query))))))
      (make-instance 'range-future :fdb-future fdb-future :range-query range-query))))
  
(defun map-range-query (result-type function transaction &rest args)
  (let ((range-query (apply #'make-range-query args))
        (result nil))
    (loop as future = (transaction-range-query-next transaction range-query)
          while (not (null future))
          do
      (unwind-protect
           (progn
             (future-block-until-ready future)
             (let ((segment (range-future-ready-map-values result-type function future)))
               (when result-type
                 (setq result (if (null result) segment (concatenate result-type result segment))))))
        (future-destroy future)))
    (when (and result-type (null result))
      (setq result (make-sequence result-type 0)))
    result))

(defmacro do-range-query (((key value) &rest args) &body body)
  `(block nil
    (map-range-query nil #'(lambda (,key ,value) . , body) . ,args)))
    
(defun transaction-range-query (transaction &rest args)
  (apply #'map-range-query 'list #'list transaction args))
