(defsystem common-doc-plump
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:common-doc
               :plump
               :anaphora)
  :components ((:module "src"
                :serial t
                :components
                ((:file "parser")
                 (:file "emitter"))))
  :description "Translate a Plump DOM into a CommonDoc document and back."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op common-plump-test))))
