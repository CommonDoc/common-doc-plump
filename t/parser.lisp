(in-package :cl-user)
(defpackage common-doc-plump-test.parser
  (:use :cl :fiveam)
  (:import-from :common-doc
                :<text-node>
                :<paragraph>
                :<bold>
                :<italic>
                :<underline>
                :<strikethrough>
                :<code>
                :<superscript>
                :<subscript>
                :<code-block>
                :<inline-quote>
                :<block-quote>
                :<document-link>
                :<web-link>
                :<list-item>
                :<definition>
                :<unordered-list>
                :<ordered-list>
                :<definition-list>
                :<image>
                :<figure>
                :<table>
                :<row>
                :<cell>
                :<section>
                :children
                :text
                :document-reference
                :section-reference
                :uri))
(in-package :common-doc-plump-test.parser)

;;; Utilities

(defmacro test-parse (xml class &rest tests)
  `(let* ((plump:*tag-dispatchers* plump:*xml-tags*)
          (plump-node (plump:parse ,xml))
          (parsed (common-doc-plump.parser:parse plump-node)))
     (is-true (typep parsed ',class))
     ,@tests))

(defun mk-tag (tag-name &optional (content "test"))
  (format nil "<~A>~A</~A>" tag-name content tag-name))

(defmacro test-tag (tag-name class)
  `(test-parse (mk-tag ,tag-name) ,class))

(defmacro test-classes (&rest classes)
  `(test trivial-tags
     ,@(loop for class in classes collecting
         `(test-tag (common-doc:find-tag (find-class ',class))
                    ,class))))

(defmacro test-child ()
  `(let ((text-node (first (children parsed))))
     (is-true (typep text-node '<text-node>))
     (is-true (text text-node) "test")))

;;; Tests

(def-suite tests
  :description "vertex tests.")
(in-suite tests)

(test trivial
  (test-parse "test" <text-node>))

(test-classes <paragraph>
              <bold>
              <italic>
              <underline>
              <strikethrough>
              <code>
              <superscript>
              <subscript>
              <inline-quote>
              <block-quote>
              <list-item>)

(test code
  (test-parse (mk-tag "code") <code-block>
    (test-child)))

(test refs
  (test-parse "<ref doc=\"document\" sec=\"section\">test</ref>"
              <document-link>
    (is (equal (document-reference parsed) "document"))
    (is (equal (section-reference parsed) "section"))
    (test-child)))

(test links
  (test-parse "<link uri=\"test\">test</link>"
              <web-link>
    (is (equal (quri:render-uri (uri parsed)) "test"))
    (test-child)))

(run! 'tests)
