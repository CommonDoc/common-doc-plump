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

(defmethod doc->xml ((node <text-node>))
  (progn
    (write-string (text node) markup:*output-stream*)
    nil))

(defmethod doc->xml ((list list))
  (loop for child in list do
    (doc->xml child)))

(defmethod doc->xml ((code <code-block>))
  (html (:code :language (language code)
               (doc->xml (children code)))))

(defmethod doc->xml ((ref <document-link>))
  (aif (document-reference ref)
       (html (:ref :sec (section-reference ref)
                   (doc->xml (children ref))))
       (html (:ref :doc it
                   :sec (section-reference ref)
                   (doc->xml (children ref))))))

(defmethod doc->xml ((link <web-link>))
  (html (:link :uri (quri:render-uri (uri link))
               (doc->xml (children link)))))

(defmethod doc->xml ((def <definition>))
  (html (:term (doc->xml (term def)))
        (:def (doc->xml (definition def)))))

(defmethod doc->xml ((item <list-item>))
  (html (:item (doc->xml (children item)))))

(defmethod doc->xml ((list <unordered-list>))
  (html (:list (doc->xml (children list)))))

(defmethod doc->xml ((list <ordered-list>))
  (html (:enum (doc->xml (children list)))))

(defmethod doc->xml ((list <definition-list>))
  (html (:deflist (doc->xml (children list)))))

(defun emit (node)
  "Produce a Plump node from a CommonDoc document."
  (plump:parse
   (with-output-to-string (stream)
     (let ((markup:*output-stream* stream))
       (doc->xml node)))))
