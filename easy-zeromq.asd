(defsystem "easy-zeromq"
  :version "1.0.1"
  :description "Wrap of pzmq with patterns ready."
  :author "He Xiang-zhi <xz.he@qq.com>"
  :license "MIT"
  :depends-on (:pzmq
               :uuid
               :usocket
               :cl-ppcre
               :ip-interfaces
               :cl-conspack
               :log4cl
               :bordeaux-threads)
  :in-order-to ((test-op (test-op "easy-zeromq/tests")))
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "patch")
                             (:file "utils")
                             (:file "encoding")
                             (:file "zeromq")))))

(defsystem "easy-zeromq/tests"
  :author "He Xiang-zhi <xz.he@qq.com>"
  :license "MIT"
  :version "1.0.1"
  :serial t
  :depends-on (:easy-zeromq
               :flexi-streams
               :trivial-backtrace
               :parachute)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "encoding")
                             (:file "zeromq"))))
  :perform (test-op (o s) (uiop:symbol-call :parachute :test :easy-zeromq-tests)))
