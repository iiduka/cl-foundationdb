;;; -*- Mode: Lisp -*-

(asdf:defsystem :foundationdb
    :description "FoundationDB Common Lisp binding"
    :version "0.0.1"
    :license "MIT"
    :depends-on (:cffi :babel)
    :components ((:file "package")
                 (:file "c-api" :depends-on ("package"))
                 (:file "base" :depends-on ("c-api"))))

(pushnew :foundationdb *features*)
