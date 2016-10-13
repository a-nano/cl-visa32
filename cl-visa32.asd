#|
  This file is a part of cl-visa32 project.
  Copyright (c) 2016 Akihide Nano (an74abc@gmail.com)
|#

#|
  Author: Akihide Nano (an74abc@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-visa32-asd
  (:use :cl :asdf))
(in-package :cl-visa32-asd)

(defsystem cl-visa32
  :version "0.1"
  :author "Akihide Nano"
  :license "LLGPL"
  :depends-on (:cffi)
  :components ((:module "src"
                :components
                ((:file "operations")
                 (:file "cl-visa32" :depends-on ("operations")))))
  :description "visa32 binding for Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-visa32-test))))
