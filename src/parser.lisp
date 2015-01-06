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
                :<image>
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
               (and (plump:element-p node)
                    (equal (plump:tag-name node) tag-name)))
           vector))

(defun tags-without-name (tag-name vector)
  (find-if-not #'(lambda (node)
                   (and (plump:element-p node)
                        (equal (plump:tag-name node) tag-name)))
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
  (let ((child (find-if #'(lambda (node)
                            (or (plump:element-p node)
                                (typep node 'plump:text-node)))
                        (plump:children root))))
    (parse child)))

(defmethod parse ((node plump:element))
  (let ((name (plump:tag-name node))
        (attributes (plump:attributes node))
        (children (plump:children node)))
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
               (make-instance 'common-doc.macro:<macro-node>
                              :name name
                              :metadata attributes
                              :children (parse children)))))))

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
    (make-instance '<document-link>
                   :document-reference doc-ref
                   :section-reference sec-ref
                   :children (parse children))))

(define-attr-parser "link" (attributes children)
  (let* ((uri-text (gethash "uri" attributes))
         (uri (quri:uri uri-text)))
    (make-instance '<web-link> :uri uri :children (parse children))))

;; Lists

(define-parser "deflist" (children)
  (make-instance '<definition-list>
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
                     (let ((term (make-instance '<content-node>
                                                :children (parse (plump:children term))))
                           (def (make-instance '<content-node>
                                               :children (parse (plump:children def)))))
                       (make-instance '<definition>
                                      :term term
                                      :definition def))))))

(define-parser "figure" (children)
  (let ((image (find-tag-by-name "image" children))
        (description (tags-without-name "image" children)))
    (make-instance '<figure>
                   :image (parse image)
                   :description
                   (make-instance '<content-node>
                                  :children (list (parse description))))))

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
