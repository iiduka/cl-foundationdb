;;; -*- Mode: Lisp -*-

(asdf:defsystem :foundationdb
    :description "FoundationDB Common Lisp binding"
    :version "0.0.1"
    :license "MIT"
    :depends-on (:cffi :babel)
    :components
    ((:module "src"
      :components ((:file "package")
                   (:file "c-api" :depends-on ("package"))
                   (:file "base" :depends-on ("c-api"))
                   (:file "tuple" :depends-on ("package"))
                   (:file "tuple-storage" :depends-on ("tuple" "base"))))))

(pushnew :foundationdb *features*)

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :foundationdb))))
  (asdf:operate 'asdf:test-op :foundationdb-tests))
