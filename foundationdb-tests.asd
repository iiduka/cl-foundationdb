;;; -*- Mode: Lisp -*-

(asdf:defsystem :foundationdb-tests
    :description "FoundationDB Common Lisp tests"
    :version "0.0.1"
    :license "MIT"
    :depends-on (:cffi :foundationdb)
    :components
    ((:module "tests"
      :components ((:file "package")
                   (:file "basic-tests" :depends-on ("package"))
                   (:file "tuple-tests" :depends-on ("basic-tests"))
                   (:file "run-tests" :depends-on ("basic-tests" "tuple-tests"))))))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :foundationdb-tests))))
  (asdf:operate 'asdf:load-op :foundationdb-tests)
  (funcall (intern (symbol-name '#:run-tests) :foundationdb-tests)))
