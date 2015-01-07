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

(defun xml->doc (xml)
  (let* ((plump:*tag-dispatchers* plump:*xml-tags*)
         (plump-node (plump:parse xml)))
    (common-doc-plump.parser:parse plump-node)))

(defmacro test-emit (xml)
  `(is
    (equal
     ,xml
     (with-output-to-string (stream)
       (plump:serialize (doc->plump (xml->doc ,xml)) stream)))))

(defmacro trivial-tests (&rest classes)
  `(progn
     ,@(loop for class in classes collecting
         `(let ((tag (find-tag (find-class ',class))))
            (test-emit (format nil "<~A>test</~A>" tag tag))))))

;;; Tests

(def-suite emitter
  :description "VerTeX emitter tests.")
(in-suite emitter)

(test text
  (test-emit "test"))

(test trivial
  (trivial-tests <paragraph>
                 <bold>
                 <italic>
                 <underline>
                 <strikethrough>
                 <code>
                 <superscript>
                 <subscript>
                 <inline-quote>
                 <block-quote>))

(test refs
  (test-emit "<ref doc=\"doc\" sec=\"sec\"/>")
  (test-emit "<ref sec=\"sec\"/>")
  (test-emit "<ref sec=\"sec\">test</ref>"))

(test links
  (test-emit "<link uri=\"http://example.com/\">test</link>"))

(test lists
  (let ((elems (list "1" "2" "3")))
    (loop for class in (list "list" "enum") do
      (test-emit
       (format nil "<~A>~{<item>~A</item>~}</~A>"
               class elems class)))))

(test definition-list
  (test-emit "<deflist><term>term</term><def>def</def></deflist>"))

(test image
  (test-emit "<image src=\"src\" desc=\"\"/>"))

(test figure
  (test-emit "<figure><image src=\"src\" desc=\"\"/>test</figure>"))

(test tables
  (test-emit "<table><row><cell>1</cell><cell>2</cell></row></table>"))
