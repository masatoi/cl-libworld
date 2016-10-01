#|
  This file is a part of cl-libworld project.
|#

(in-package :cl-user)
(defpackage cl-libworld-test-asd
  (:use :cl :asdf))
(in-package :cl-libworld-test-asd)

(defsystem cl-libworld-test
  :author ""
  :license ""
  :depends-on (:cl-libworld
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-libworld"))))
  :description "Test system for cl-libworld"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
