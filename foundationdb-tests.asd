;;; -*- Mode: Lisp -*-

(asdf:defsystem :foundationdb-tests
    :description "FoundationDB Common Lisp tests"
    :version "0.0.1"
    :license "MIT"
    :depends-on (:foundationdb)
    :components
    ((:module "tests"
      :components ((:file "package")
                   (:file "basic-tests" :depends-on ("package"))
                   (:file "tuple-tests" :depends-on ("basic-tests"))
                   (:file "directory-tests" :depends-on ("tuple-tests"))
                   (:file "counter-tests" :depends-on ("basic-tests"))
                   (:file "run-tests" :depends-on ("basic-tests" "tuple-tests" "counter-tests"))))))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :foundationdb-tests))))
  (asdf:operate 'asdf:load-op :foundationdb-tests)
  (funcall (intern (symbol-name '#:run-tests) :foundationdb-tests)))
