;;; -*- Mode: Lisp -*-

(in-package :foundationdb)

(defparameter *little-endian-long-one* (coerce '(1 0 0 0 0 0 0 0) '(array (unsigned-byte 8) (*))))
(defparameter *default-node-subspace-prefix* (coerce '(#xFE) '(array (unsigned-byte 8) (*))))
(defparameter *high-contention-key* (map '(array (unsigned-byte 8) (*)) #'char-code "hca"))
(defparameter *layer-key* (map '(array (unsigned-byte 8) (*)) #'char-code "layer"))
(defparameter *sub-dir-key* 0)

(defparameter *default-node-subspace* (make-subspace :raw-prefix *default-node-subspace-prefix*))
(defparameter *default-contents-subspace* (make-subspace))

;;; DIRECTORY is already spoken for as a function.
(defclass root-directory ()
  ((root-node)
   (node-subspace :initarg :node-subspace :initform *default-node-subspace*)
   (content-subspace :initarg :content-subspace :initform *default-contents-subspace*)
   (allocator)))

(defmethod initialize-instance :after ((obj root-directory) &key)
  (with-slots (root-node node-subspace allocator) obj
    ;; The root node is the one whose contents are the node subspace
    (setf root-node (subspace node-subspace (key-bytes node-subspace))
          allocator (make-instance 'high-contention-allocator
                                   :subspace (subspace root-node *high-contention-key*)))))

(defun make-root-directory (&rest args)
  (apply #'make-instance 'root-directory args))

(defclass directory-subspace (subspace)
  ((path :initarg :path)
   (layer :reader directory-subspace-layer :initarg :layer :initform nil)
   (directory :initarg :directory)))

(defgeneric directory-subspace-path (directory-subspace)
  (:method ((dir root-directory)) (make-tuple))
  (:method ((dir directory-subspace)) (slot-value dir 'path)))

(defgeneric directory-subspace-exists-p (directory-subspace tr &optional sub-path)
  (:method (dir (db database) &optional sub-path)
           (with-transaction (tr db)
             (directory-subspace-exists-p dir tr sub-path)))
  (:method ((dir directory-subspace) (tr transaction) &optional sub-path)
           (with-slots (directory path) dir
             (directory-subspace-exists-p-internal directory (combine-paths path sub-path) tr)))
  (:method ((dir root-directory) (tr transaction) &optional sub-path)
           (or (null sub-path)
               (directory-subspace-exists-p-internal dir sub-path tr))))

(defgeneric directory-subspace-open (directory-subspace tr sub-path
                                     &key if-exists if-does-not-exist layer prefix)
  (:method (dir (db database) sub-path &rest args)
           (with-transaction (tr db)
             (apply #'directory-subspace-open dir tr sub-path args)))
  (:method ((dir directory-subspace) (tr transaction) sub-path &rest args)
           (with-slots (directory path) dir
             (apply #'directory-subspace-open-internal directory (combine-paths path sub-path) tr args)))
  (:method ((dir root-directory) (tr transaction) sub-path &rest args)
           (if (null sub-path)
               dir
               (apply #'directory-subspace-open-internal dir sub-path tr args))))

(defgeneric directory-subspace-rename (directory-subspace tr path1 &optional path2)
  (:method (dir (db database) path1 &optional path2)
           (with-transaction (tr db)
             (directory-subspace-rename dir tr path1 path2)))
  (:method ((dir directory-subspace) (tr transaction) path1 &optional path2)
           (with-slots (directory path) dir
             (let (from to)
               (if (null path2)
                   (setq from path to path1)
                   (setq from (combine-paths path path1) to path2))
               (directory-subspace-rename-internal directory from to tr))))
  (:method ((dir root-directory) (tr transaction) path1 &optional path2)
           (when (null path2)
             (error "Cannot rename root directory as sub-space"))
           (directory-subspace-rename-internal dir path1 path2 tr)))

(defgeneric directory-subspace-delete (directory-subspace tr
                                       &key sub-path if-does-not-exist)
  (:method (dir (db database) &rest args)
           (with-transaction (tr db)
             (apply #'directory-subspace-delete dir tr args)))
  (:method ((dir directory-subspace) (tr transaction) &rest args &key sub-path &allow-other-keys)
           (with-slots (directory path) dir
             (apply #'directory-subspace-delete-internal directory (combine-paths path sub-path) tr args)))
  (:method ((dir root-directory) (tr transaction) &rest args &key sub-path &allow-other-keys)
           (when (null sub-path)
             (error "Cannot delete root directory as sub-space"))
           (apply #'directory-subspace-delete-internal dir sub-path tr args)))

(defgeneric directory-subspace-list (directory-subspace tr &optional sub-path)
  (:method (dir (db database) &optional sub-path)
           (with-transaction (tr db)
             (directory-subspace-list dir tr sub-path)))
  (:method ((dir directory-subspace) (tr transaction) &optional sub-path)
           (with-slots (directory path) dir
             (directory-subspace-list-internal directory (combine-paths path sub-path) tr)))
  (:method ((dir root-directory) (tr transaction) &optional sub-path)
           (directory-subspace-list-internal dir (or sub-path (make-tuple)) tr)))

(defun combine-paths (path sub-path)
  (if (null sub-path)
      (or path (make-tuple))
      (let ((combined (copy-tuple path)))
        (dotimes (i (length sub-path))
          (tuple-push combined (tuple-elt sub-path i)))
        combined)))

(defun directory-subspace-exists-p-internal (dir path tr)
  (not (null (find-node dir tr path))))

(defun directory-subspace-open-internal (dir path tr 
                                         &key sub-path
                                              if-exists (if-does-not-exist :error)
                                              layer prefix)
  (declare (ignore sub-path))
  (let ((existing-node (find-node dir tr path)))
    (if existing-node
        (ecase if-exists
          (:error
           (error "Directory ~S already exists" path))
          ((nil)
           (let ((existing-layer 
                  (future-value 
                   (transaction-get tr 
                                    (subspace-encode-key existing-node *layer-key*)))))
             (check-layer path layer existing-layer)
             (contents-of-node dir existing-node path existing-layer))))
        (ecase if-does-not-exist
          (:error
           (error "Directory ~S does not exist" path))
          (:create
           (when (null prefix)
             (setq prefix (hca-allocate (slot-value dir 'allocator))))
           (unless (prefix-free-p dir tr prefix)
             (error "Prefix ~S already in use" prefix))
           (multiple-value-bind (parent-path name)
               (split-path path)
             (let ((parent-node (if parent-path
                                    (node-with-prefix dir parent-path)
                                    (slot-value dir 'root-node))))
               (transaction-set tr
                                (subspace-encode-key (subspace parent-node *sub-dir-key*)
                                                     name)
                                prefix)
             (let ((node (node-with-prefix dir prefix)))
               (when layer
                 (transaction-set tr (subspace-encode-key node *layer-key*) layer))
               (contents-of-node dir node path layer)))))
          ((nil)
           nil)))))

(defun directory-subspace-rename-internal (dir old-path new-path tr)
  (when (find-node dir tr new-path)
    (error "Directory ~S already exists" new-path))
  (let ((old-node (find-node dir tr old-path)))
    (when (null old-node)
      (error "Directory ~S does not exist" old-path))
    (multiple-value-bind (parent-path name)
        (split-path new-path)
      (let ((parent-node (find-node dir tr parent-path)))
        (when (null parent-node)
          (error "Parent directory of ~S does not exist" new-path))
        (transaction-set tr
                         (subspace-encode-key (subspace parent-node *sub-dir-key*) name)
                         (contents-of-node dir old-node nil nil))))
    (remove-from-parent dir tr old-path)
    (contents-of-node dir old-node new-path 
                      (future-value
                       (transaction-get tr (subspace-encode-key old-node *layer-key*))))))
      
(defun directory-subspace-delete-internal (dir path tr
                                           &key sub-path (if-does-not-exist :error))
  (declare (ignore sub-path))
  (let ((node (find-node dir tr path)))
    (if (null node)
        (ecase if-does-not-exist
          (:error (error "Directory ~S does not exist" path))
          ((nil) nil))
        (progn
          (remove-recursive dir tr node)
          (remove-from-parent dir tr path)
          t))))
  
(defun directory-subspace-list-internal (dir path tr)
  (let ((node (find-node dir tr path)))
    (when (null node)
      (error "Directory ~S does not exist" path))
    (loop for nn in (list-sub-dirs dir tr node)
          collect (first nn))))

(defun node-with-prefix (dir prefix)
  (and prefix (subspace (slot-value dir 'node-subspace) prefix)))

(defun contents-of-node (dir node path layer)
  (let ((prefix (tuple-elt (subspace-decode-key (slot-value dir 'node-subspace)
                                                (key-bytes node)) 
                           0)))
    ;; TODO: What about content-subspace? See community site bug report.
    (make-instance 'directory-subspace :directory dir :path path 
                                       :prefix prefix :layer layer)))

(defun find-node (dir tr path)
  (let ((node (slot-value dir 'root-node)))
    (dotimes (i (tuple-length path))
      (let ((prefix (subspace-encode-key (subspace node *sub-dir-key*)
                                         (tuple-elt path i))))
        (setq node (node-with-prefix dir (future-value (transaction-get tr prefix)))))
      (when (null node)
        (return-from find-node nil)))
    node))

(defun remove-from-parent (dir tr path)
  (multiple-value-bind (parent-path name)
      (split-path path)
    (let ((parent (find-node dir tr (tuple-subtuple path 0 parent-path))))
      (transaction-clear tr 
                         (subspace-encode-key (subspace parent *sub-dir-key*) name)))))

(defun remove-recursive (dir tr node)
  (dolist (sub-node (list-sub-dirs dir tr node))
    (remove-recursive dir tr (second sub-node)))
  (transaction-clear tr (range-starts-with (contents-of-node dir node nil nil)))
  (transaction-clear tr (subspace-range node)))

(defun split-path (path)
  (let ((last (1- (tuple-length path))))
    (values (and (plusp last) (tuple-subtuple path 0 last))
            (tuple-elt path last))))

(defun check-layer (path stored opened)
  (when (and stored opened (not (equalp stored opened)))
    (error "Directory ~S created with incompatible layer" path)))

(defun prefix-free-p (dir tr prefix)
  t)

(defun list-sub-dirs (dir tr node)
  (let ((sd (subspace node *sub-dir-key*)))
    (map-range-query 'list 
                     #'(lambda (key value)
                         (list (tuple-elt (subspace-decode-key sd key) 0)
                               (node-with-prefix dir value)))
                     tr (subspace-range sd))))

(defclass high-contention-allocator ()
  ((subspace :initarg :subspace)))

(defun hca-allocate (allocator)
  )
