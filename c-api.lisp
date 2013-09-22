;;; -*- Mode: Lisp -*-

(in-package :foundationdb)

(define-foreign-library libfdb
  (:unix "libfdb_c.so")
  (t "libfdb_c"))

(use-foreign-library libfdb)

(defctype fdb-error :int)

(defcfun "fdb_select_api_version_impl" fdb-error
  (runtime-version :int)
  (header-version :int))

(defctype fdb-future :pointer)
(defctype fdb-cluster :pointer)
(defctype fdb-database :pointer)

(defcfun "fdb_create_cluster" fdb-future
  (cluster-file-path :string))

(defcfun "fdb_future_block_until_ready" fdb-error
  (future fdb-future))

(defcfun "fdb_future_destroy" fdb-error
  (future fdb-future))

(defcfun "fdb_future_get_error" fdb-error
  (future fdb-future))

(defcfun "fdb_future_get_cluster" fdb-error
  (future fdb-future)
  (pcluster (:pointer fdb-cluster)))

(defcfun "fdb_cluster_create_database" fdb-future
  (cluster fdb-cluster)
  (db-name (:pointer :unsigned-char))
  (db-name-length :int))

(defcfun "fdb_future_get_database" fdb-error
  (future fdb-future)
  (pdatabase (:pointer fdb-database)))

(defcfun "fdb_setup_network" fdb-error)

(defcfun "fdb_run_network" fdb-error)

(defcfun "fdb_stop_network" fdb-error)
