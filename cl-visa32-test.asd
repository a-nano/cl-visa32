#|
  This file is a part of cl-visa32 project.
  Copyright (c) 2016 Akihide Nano (an74abc@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-visa32-test-asd
  (:use :cl :asdf))
(in-package :cl-visa32-test-asd)

(defsystem cl-visa32-test
  :author "Akihide Nano"
  :license "LLGPL"
  :depends-on (:cl-visa32
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-visa32"))))
  :description "Test system for cl-visa32"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
