(in-package :cl-user)
(defpackage common-doc-plump-test.parser
  (:use :cl :fiveam :common-doc)
  (:export :parser))
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
     (is-true (typep text-node 'text-node))
     (is-true (text text-node) "test")))

;;; Tests

(def-suite parser
  :description "VerTeX parser tests.")
(in-suite parser)

(test trivial
  (test-parse "test" text-node))

(test-classes bold
              italic
              underline
              strikethrough
              code
              superscript
              subscript
              inline-quote
              block-quote
              list-item)

(test code
  (test-parse (mk-tag "code") code-block
    (test-child)))

(test refs
  (test-parse "<ref doc='document' sec='section'>test</ref>"
              document-link
    (is (equal (document-reference parsed) "document"))
    (is (equal (section-reference parsed) "section"))
    (test-child)))

(test links
  (test-parse "<link uri='test'>test</link>"
              web-link
    (is (equal (quri:render-uri (uri parsed)) "test"))
    (test-child)))

(test list
  (loop for list-type in '("list" "enum") do
    (let* ((elems (list "test" "test" "test"))
           (list-xml (format nil "<~A>~{<item>~A</item>~}</~A>"
                             list-type elems list-type)))
      (test-parse list-xml base-list
        (loop for child in (children parsed) do
          (is-true (typep child 'list-item))
          (is (equal (text (first (children child))) "test")))))))

(test image
  (test-parse "<image src='src' desc='desc'/>"
              image
    (is (equal (source parsed) "src"))
    (is (equal (description parsed) "desc"))))

(test figure
  (test-parse "<figure><image src='src' desc='desc'/>desc</figure>"
              figure
  (let ((image (image parsed))
        (desc (description parsed)))
    (is-true (typep image 'image))
    (is (equal (source image) "src"))
    (is (equal (description image) "desc"))
    (is (equal (text (first (children desc)))
               "desc")))))

(test table
  (test-parse "<table><row><cell>1</cell><cell>2</cell></row></table>"
              table
    (is (equal (length (rows parsed))
               1))
    (let ((first-row (first (rows parsed))))
      (is-true (typep first-row 'row))
      (is (equal (length (cells first-row)) 2))
      (let ((first-cell (first (cells first-row))))
        (is (equal (text (first (children first-cell)))
                   "1"))))))

(test section
  (test-parse "<section title='title'>test</section>"
              section
    (is (equal (text (title parsed))
               "title"))
    (is (equal (text (first (children parsed)))
               "test"))))
