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

(defmacro parse-doc ((doc xml) &rest tests)
  `(let* ((plump:*tag-dispatchers* plump:*xml-tags*)
          (plump-node (plump:parse ,xml))
          (,doc (common-doc-plump.parser:parse-document plump-node)))
     ,@tests))

;;; Tests

(def-suite parser
  :description "common-doc-plump parser tests.")
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
  (test-parse (mk-tag "code")
              code-block
    (test-child)))

(test refs
  (test-parse "<ref doc='document' sec='section'>test</ref>"
              document-link
    (is (equal (document-reference parsed) "document"))
    (is (equal (node-reference parsed) "section"))
    (test-child))
  (test-parse "<ref doc='document' id='section'>test</ref>"
              document-link
    (is (equal (document-reference parsed) "document"))
    (is (equal (node-reference parsed) "section"))
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
      (test-parse list-xml
                  base-list
        (loop for child in (children parsed) do
          (is-true (typep child 'list-item))
          (is (equal (text (first (children child))) "test")))))))

(test deflist
  (test-parse "<deflist><term>term</term><def>def</def></deflist>"
              definition-list
    (is (equal (length (children parsed))
               1))))

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
    (is (equal (text (first (title parsed)))
               "title"))
    (is (equal (text (first (children parsed)))
               "test"))))

(test nested-section
  (test-parse "<section title='parent'><section title='child'>test</section></section>"
              section
    (is (equal (text (first (title parsed)))
               "parent"))
    (is (equal (text (first (title (first (children parsed)))))
               "child"))
    (is (equal (text (first (children (first (children parsed)))))
               "test"))))

(test section-with-title-node
  (test-parse "<section><title>title</title>test</section>"
              section
    (is (equal (text (first (title parsed)))
               "title"))
    (is (equal (length (children parsed))
               1))
    (is (equal (text (first (children parsed)))
               "test"))))

(test section-with-bold-title
  (test-parse "<section><title>title <b>bold</b></title>test</section>"
              section
    (is (equal (length (title parsed))
               2))
    (is (equal (text (first (title parsed)))
               "title "))
    (is (equal (text (first (children (second (title parsed)))))
               "bold"))
    (is (equal (length (children parsed))
               1))
    (is (equal (text (first (children parsed)))
               "test"))))

(test real-world-list
  (test-parse
   "<list>
      <item>1</item>
      <item>2</item>
      <item>3</item>
   </list>"
   unordered-list
   (let ((children (children parsed)))
     (is (equal (length children)
                3))
     (loop for child in children do
       (is-true (typep child 'list-item)))
     (is (equal (text (first (children (first children))))
         "1"))
     (is (equal (text (first (children (second children))))
         "2"))
     (is (equal (text (first (children (third children))))
         "3")))))

(test real-world-deflist
  (test-parse
   "<deflist>
      <term>a</term>
      <def>1</def>

      <term>b></term>
      <def>2</def>
    </deflist>"
   definition-list
   (let ((defs (children parsed)))
     (is (equal (length defs)
                2)))))

(test parse-document
  (parse-doc (doc "<title>a</title>derp")
    (is
     (equal (title doc)
            "a"))))
