(in-package :cl-user)
(defpackage common-doc-plump-test.parser
  (:use :cl :fiveam))
(in-package :common-doc-plump-test.parser)

;;; Utilities

(defmacro test-parse ((plump-class &rest plump-attributes)
                      (common-doc-class))
  `(let* ((node (make-instance ',plump-class
                               :parent nil
                               ,@plump-attributes))
          (parsed (common-doc-plump.parser:parse node)))
     (is-true (typep parsed ',common-doc-class))))

;;; Tests

(def-suite tests
  :description "vertex tests.")
(in-suite tests)

(test trivial
  (test-parse (plump:text-node :text "test")
      (common-doc:<text-node>)))

(run! 'tests)
