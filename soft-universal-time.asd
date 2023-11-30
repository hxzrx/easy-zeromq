(defsystem "soft-universal-time"
  :version "1.0.0"
  :description "Soft Universal Time, get universal time from a global variable "
  :author "He Xiang-zhi <xz.he@qq.com>"
  :license "MIT"
  :depends-on (:local-time
               :bordeaux-threads)
  :in-order-to ((test-op (test-op "soft-universal-time/tests")))
  :components ((:file "time")))

(defsystem "soft-universal-time/tests"
  :author "He Xiang-zhi <xz.he@qq.com>"
  :license "MIT"
  :version "1.0.0"
  :serial t
  :depends-on (:soft-universal-time
               :parachute)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "time"))))
  :perform (test-op (o s) (uiop:symbol-call :parachute :test :soft-universal-time-tests)))
