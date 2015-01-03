(in-package :cl-user)
(defpackage common-doc-plump.parser
  (:use :cl :anaphora)
  (:import-from :common-doc
                :<content-node>
                :<text-node>
                :<code-block>
                :<document-link>
                :<web-link>
                :<definition>
                :<unordered-list>
                :<ordered-list>
                :<definition-list>
                :<figure>
                :<section>
                :<document>)
  (:export :parse)
  (:documentation "Parse a Plump document into a CommonDoc document."))
(in-package :common-doc-plump.parser)

;;; Variables

(defparameter *parsers* (make-hash-table :test #'equal))

;;; Utilities

(defun find-tag-by-name (tag-name vector)
  (find-if #'(lambda (node)
               (equal (plump:tag-name node) tag-name))
           vector))

(defun tags-without-name (tag-name vector)
  (find-if-not #'(lambda (node)
                   (equal (plump:tag-name node) tag-name))
               vector))

;;; Methods

(defgeneric parse (obj)
  (:documentation "Parse a plump-tex node into a CommonDoc node."))

(defmethod parse ((node plump:text-node))
  (make-instance '<text-node>
                 :text (plump:text node)))

(defmethod parse ((vec vector))
  (loop for elem across vec collecting
    (parse elem)))

(defmethod parse ((root plump:root))
  (make-instance '<content-node>
                 :children (parse (plump:children root))))

(defmethod parse ((node plump:element))
  (let ((name (plump:tag-name node))
        (attributes (plump:attributes node))
        (children (plump:children node)))
    (aif (gethash name *parsers*)
         (funcall it attributes children)
         (let ((tag-class (common-doc:find-node name)))
           (if tag-class
               (make-instance tag-class
                              :metadata attributes
                              :children (parse children))
               (make-instance 'common-doc.macro:<macro-node>
                              :name name
                              :metadata attributes
                              :children (parse children)))))))

;;; Parsers

(defmacro define-attr-parser (name (attrs args) &rest body)
  `(setf (gethash ,name *parsers*)
         #'(lambda (,attrs ,args)
             ,@body)))

(defmacro define-parser (name (args) &rest body)
  `(setf (gethash ,name *parsers*)
         #'(lambda (attrs ,args)
             (declare (ignore attrs))
             ,@body)))

;; Code blocks

(define-attr-parser "code" (attributes children)
  (let ((language (gethash "language" attributes)))
    (make-instance '<code-block>
                   :language language
                   :children (parse children))))

;; Links

(define-attr-parser "ref" (attributes children)
  (let ((sec-ref (gethash "sec" attributes))
        (doc-ref (gethash "doc" attributes)))
    (make-instance '<document-link>
                   :section-reference sec-ref
                   :document-reference doc-ref
                   :children (parse children))))

(define-attr-parser "link" (attributes children)
  (let* ((uri-text (gethash "uri" attributes))
         (uri (quri:uri uri-text)))
    (make-instance '<web-link> :uri uri :children (parse children))))

;; Lists

(define-parser "def" (children)
  (let ((term (find-tag-by-name "term" children))
        (definition (tags-without-name "term" children)))
    (make-instance '<definition>
                   :term (parse term)
                   :definition (make-instance '<content-node>
                                              :children (parse definition)))))

(define-parser "figure" (children)
  (let ((image (find-tag-by-name "image" children))
        (description (tags-without-name "image" children)))
    (make-instance '<figure>
                   :image image
                   :description
                   (make-instance '<content-node>
                                  :children (parse description)))))

;; Structure

(define-attr-parser "section" (attributes children)
  (let ((title (aif (gethash "title" attributes)
                    ;; We got the title from the attributes
                    (make-instance '<text-node>
                                   :text it)
                    ;; Otherwise, look for a title tag in the children
                    (aif (find-tag-by-name "title" children)
                         (parse it)
                         (error "Untitled section."))))
        (children (tags-without-name "title" children))
        (reference (gethash "ref" attributes)))
    (make-instance '<section> :title title :reference reference :children children)))
