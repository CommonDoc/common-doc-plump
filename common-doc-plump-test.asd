(defsystem common-doc-plump-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :description "Tests for common-doc-plump"
  :depends-on (:common-doc-plump
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "parser")
                 (:file "emitter")
                 (:file "common-doc-plump")))))
