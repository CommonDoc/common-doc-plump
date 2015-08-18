(in-package :cl-user)
(defpackage common-doc-plump.emitter
  (:use :cl :anaphora :common-doc)
  (:export :emit)
  (:documentation "Emit a Plump DOM from a CommonDoc document."))
(in-package :common-doc-plump.emitter)

;; We take the easy way out, and instead of translating the DOM, simply dump the
;; CommonDoc to XML and parse it into a Plump DOM.

(defmacro html (&rest body)
  "A wrapper around cl-markup's `markup` macro which works better in recursive
contexts."
  `(progn
     (markup:markup ,@body)
     nil))

(defgeneric doc->xml (node)
  (:documentation "Turn a CommonDoc node to XML."))

(defmethod doc->xml ((node text-node))
  (progn
    (write-string (text node) markup:*output-stream*)
    nil))

(defmethod doc->xml ((list list))
  (loop for child in list do
    (doc->xml child)))

(defmacro define-trivial-emitters (&rest classes)
  `(progn
    ,@(loop for class in classes collecting
        `(defmethod doc->xml ((node ,class))
           (let ((tag (find-tag (find-class ',class))))
             (write-string (format nil "<~A>" tag)
                           markup:*output-stream*)
             (doc->xml (children node))
             (write-string (format nil "</~A>" tag)
                           markup:*output-stream*)
             nil)))))

(define-trivial-emitters
  paragraph
  bold
  italic
  underline
  strikethrough
  code
  superscript
  subscript
  inline-quote
  block-quote)

(defmethod doc->xml ((content content-node))
  (doc->xml (children content)))

(defmethod doc->xml ((code code-block))
  (html (:code :language (language code)
               (doc->xml (children code)))))

(defmethod doc->xml ((ref document-link))
  (aif (document-reference ref)
       (html (:ref :doc it
                   :id (node-reference ref)
                   (doc->xml (children ref))))
       (html (:ref :id (node-reference ref)
                   (doc->xml (children ref))))))

(defmethod doc->xml ((link web-link))
  (html (:link :uri (quri:render-uri (uri link))
               (doc->xml (children link)))))

(defmethod doc->xml ((item list-item))
  (html (:item (doc->xml (children item)))))

(defmethod doc->xml ((def definition))
  (html (:term (doc->xml (term def)))
        (:def (doc->xml (definition def)))))

(defmethod doc->xml ((list unordered-list))
  (html (:list (doc->xml (children list)))))

(defmethod doc->xml ((list ordered-list))
  (html (:enum (doc->xml (children list)))))

(defmethod doc->xml ((list definition-list))
  (html (:deflist (doc->xml (children list)))))

;;; Figures

(defmethod doc->xml ((image image))
  (html (:image :src (source image)
                :desc (description image)
                "")))

(defmethod doc->xml ((figure figure))
  (html (:figure
         (doc->xml (image figure))
         (doc->xml (description figure)))))

;;; Tables

(defmethod doc->xml ((table table))
  (html (:table
         (doc->xml (rows table)))))

(defmethod doc->xml ((row row))
  (html (:row
         (doc->xml (cells row)))))

(defmethod doc->xml ((cell cell))
  (html (:cell
         (doc->xml (children cell)))))

;;; Emitter

(defun emit (node)
  "Produce a Plump node from a CommonDoc document."
  (let ((plump:*tag-dispatchers* plump:*xml-tags*))
    (plump:parse
     (with-output-to-string (stream)
       (let ((markup:*output-stream* stream))
         (doc->xml node))))))
