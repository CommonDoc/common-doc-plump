(in-package :cl-user)
(defpackage common-doc-plump.parser
  (:use :cl :anaphora)
  (:import-from :common-doc
                :content-node
                :text-node
                :code-block
                :document-link
                :web-link
                :definition
                :unordered-list
                :ordered-list
                :list-item
                :definition-list
                :image
                :figure
                :row
                :table
                :section
                :document
                :children)
  (:export :parse
           :parse-document
           :find-tag-by-name
           :tags-without-name
           :*serializer*)
  (:documentation "Parse a Plump document into a CommonDoc document."))
(in-package :common-doc-plump.parser)

;;; Variables

(defparameter *parsers* (make-hash-table :test #'equal))

(defvar *serializer* nil)

;;; Utilities

(defun find-tag-by-name (tag-name vector)
  "Find a Plump tag in a vector by tag name."
  (find-if #'(lambda (node)
               (and (plump:element-p node)
                    (equal (plump:tag-name node) tag-name)))
           vector))

(defun tags-without-name (tag-name-or-list vector)
  "Return a list of Plump tags in a vector whose tag name is not equal to
`tag-name-or-list`, which can be a string or a list of strings."
  (loop for elem across
                 (remove-if #'(lambda (node)
                                (if (plump:element-p node)
                                    (if (stringp tag-name-or-list)
                                        (equal (plump:tag-name node)
                                               tag-name-or-list)
                                        (every #'(lambda (tag-name)
                                                   (equal (plump:tag-name node)
                                                          tag-name))
                                               tag-name-or-list))
                                    nil))
                            vector)
        collecting elem))

(defun trim-whitespace (string)
  "Trim whitespace from a string."
  (string-trim '(#\Newline #\Space #\Tab) string))

;;; Methods

(defgeneric parse (obj)
  (:documentation "Parse a plump-tex node into a CommonDoc node."))

(defmethod parse ((node plump:text-node))
  "Parse a Plump text node."
  (let ((text (trim-whitespace (plump:text node))))
    (if (equal text "")
        nil
        (make-instance 'text-node
                       :text (plump:text node)))))

(defmethod parse ((vec vector))
  "Parse a vector of Plump nodes."
  (remove-if #'null
             (loop for elem across vec collecting
               (parse elem))))

(defmethod parse ((list list))
  "Parse a list."
  (remove-if #'null
             (loop for elem in list collecting
                                    (parse elem))))

(defmethod parse ((root plump:root))
  "Parse a Plump root element."
  (let* ((children (parse (tags-without-name (list "title")
                                             (plump:children root))))
         (root (make-instance 'content-node
                              :children children))
         (doc-with-paragraphs (common-doc.split-paragraphs:split-paragraphs root)))
    (if (rest (children doc-with-paragraphs))
        doc-with-paragraphs
        (first (children doc-with-paragraphs)))))

(defun parse-verbatim (node)
  "Parse a node with verbatim text."
  (make-instance 'text-node
                 :text (with-output-to-string (stream)
                         (funcall *serializer*
                                  (plump:make-root
                                   (plump:children node))
                                  stream))))

(defmethod parse ((node plump:element))
  "Parse a Plump element."
  (let ((name (plump:tag-name node))
        (attributes (plump:attributes node))
        (children (plump:children node)))
    (if (equal name "verb")
        ;; Verbatim input
        (parse-verbatim node)
        (aif (gethash name *parsers*)
             (funcall it attributes children)
             (let* ((tag-class (common-doc:find-node name))
                    (special-slots (common-doc:find-special-slots tag-class)))
               (if tag-class
                   (let ((instance (if (> (length children) 0)
                                       (make-instance tag-class
                                                      :children (parse children))
                                       (make-instance tag-class))))
                     (when special-slots
                       (loop for (attr-name . slot-name) in special-slots do
                         (setf (slot-value instance slot-name)
                               (gethash attr-name attributes))))
                     instance)
                   (make-instance 'common-doc.macro:macro-node
                                  :name name
                                  :metadata attributes
                                  :children (parse children))))))))

(defun parse-document (node)
  "Parse a Plump node into a document."
  (let* ((title (find-tag-by-name "title" (plump:children node)))
         (children (parse node)))
    (make-instance 'document
                   :title (if title
                              (plump:text title)
                              "")
                   :children (if (listp children)
                                 children
                                 (list children)))))
;;; Parsers

(defmacro define-attr-parser (name (attrs args) &rest body)
  `(setf (gethash ,name *parsers*)
         #'(lambda (,attrs ,args)
             ,@body)))

(defmacro define-attr-only-parser (name (attrs) &rest body)
  `(setf (gethash ,name *parsers*)
         #'(lambda (,attrs children)
             (declare (ignore children))
             ,@body)))

(defmacro define-parser (name (args) &rest body)
  `(setf (gethash ,name *parsers*)
         #'(lambda (attrs ,args)
             (declare (ignore attrs))
             ,@body)))

;; Links

(define-attr-parser "ref" (attributes children)
  (let ((doc-ref (gethash "doc" attributes))
        (sec-ref (gethash "sec" attributes)))
    (make-instance 'document-link
                   :document-reference doc-ref
                   :section-reference sec-ref
                   :children (parse children))))

(define-attr-parser "link" (attributes children)
  (let* ((uri-text (gethash "uri" attributes))
         (uri (quri:uri uri-text)))
    (make-instance 'web-link :uri uri :children (parse children))))

;; Lists

(define-parser "deflist" (children)
  (make-instance 'definition-list
                 :children
                 (let* ((children-vector
                          (remove-if-not
                           #'(lambda (child)
                               (plump:element-p child))
                           children))
                        (element-children
                          (loop for elem across children-vector
                            collecting elem)))
                   (loop for (term def) on element-children
                         by #'cddr
                         collecting
                     (let ((term (parse (plump:children term)))
                           (def (parse (plump:children def))))
                       (make-instance 'definition
                                      :term term
                                      :definition def))))))

(define-parser "list" (children)
  (make-instance 'unordered-list
                 :children
                 (let ((children-vector
                         (remove-if-not
                          #'(lambda (child)
                              (plump:element-p child))
                          children)))
                   (loop for elem across children-vector collecting
                     (make-instance 'list-item
                                    :children (parse (plump:children elem)))))))

(define-parser "enum" (children)
  (make-instance 'ordered-list
                 :children
                 (let ((children-vector
                         (remove-if-not
                          #'(lambda (child)
                              (plump:element-p child))
                          children)))
                   (loop for elem across children-vector collecting
                     (make-instance 'list-item
                                    :children (parse (plump:children elem)))))))

(define-parser "figure" (children)
  (let ((image (find-tag-by-name "image" children))
        (description (tags-without-name "image" children)))
    (make-instance 'figure
                   :image (parse image)
                   :description (parse description))))

;; Tables

(define-parser "table" (rows)
  (make-instance 'table
                 :rows (parse rows)))

(define-parser "row" (cells)
  (make-instance 'row
                 :cells (parse cells)))

;; Structure

(define-attr-parser "section" (attributes children)
  (let ((title (aif (gethash "title" attributes)
                    ;; We got the title from the attributes
                    (list (make-instance 'text-node
                                         :text it))
                    ;; Otherwise, look for a title tag in the children
                    (aif (find-tag-by-name "title" children)
                         (parse (plump:children it))
                         (error "Untitled section."))))
        (children (tags-without-name "title" children))
        (reference (gethash "ref" attributes)))
    (make-instance 'section :title title
                            :reference reference
                            :children (parse children))))
