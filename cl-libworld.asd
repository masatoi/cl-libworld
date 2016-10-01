#|
  This file is a part of cl-libworld project.
|#

(in-package :cl-user)
(defpackage cl-libworld-asd
  (:use :cl :asdf))
(in-package :cl-libworld-asd)

(defsystem cl-libworld
  :version "0.1"
  :author "Satoshi Imai"
  :license "MIT License"
  :depends-on (:cffi :lake :uiop)
  :components ((:module "src"
                :components
                ((:file "build")
                 (:file "cl-libworld"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.org"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-libworld-test))))
