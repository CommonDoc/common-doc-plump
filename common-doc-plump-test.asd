(defsystem common-doc-plump-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:common-doc-plump
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "parser")))))
