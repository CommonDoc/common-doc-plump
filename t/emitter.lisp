(in-package :cl-user)
(defpackage common-doc-plump-test.emitter
  (:use :cl :fiveam :common-doc)
  (:import-from :common-doc.util
                :make-text)
  (:export :emitter))
(in-package :common-doc-plump-test.emitter)

;;; Utilities

(defun doc->plump (doc)
  (common-doc-plump.emitter:emit doc))

(defmacro test-emit (document xml)
  `(is
    (equal
     (with-output-to-string (stream)
       (plump:serialize (doc->plump ,document) stream))
     ,xml)))

;;; Tests

(def-suite emitter
  :description "VerTeX emitter tests.")
(in-suite emitter)

(test text
  (test-emit (make-text "test") "test"))

(test lists
  (let ((elems (list "1" "2" "3")))
    (loop for class in (list (cons '<unordered-list> "list")
                             (cons '<ordered-list> "enum")) do
      (test-emit
       (make-instance (first class)
                      :children
                      (loop for elem in elems collecting
                         (make-instance '<list-item>
                                        :children (list (make-text elem)))))
       (format nil "<~A>~{<item>~A</item>~}</~A>"
               (rest class) elems (rest class))))))

(test definition
  (test-emit (make-instance '<definition>
                            :term (make-text "term")
                            :definition (make-text "def"))
             "<term>term</term><def>def</def>"))

(test definition-list
  (test-emit (make-instance '<definition-list>
                            :children
                            (list
                             (make-instance '<definition>
                                            :term (make-text "term")
                                            :definition (make-text "def"))))
             "<deflist><term>term</term><def>def</def></deflist>"))
