(in-package :cl-user)
(defpackage common-doc-plump.emitter
  (:use :cl :anaphora)
  (:import-from :common-doc
                ;; Classes
                :<text-node>
                :<code-block>
                :<document-link>
                :<web-link>
                :<definition>
                :<list>
                :<image>
                :<figure>
                ;; Slots
                :text
                :children
                :language
                :document-reference
                :section-reference
                :uri
                :term
                :definition)
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

(defmethod doc->xml ((list <list>))
  (let ((list-tag (common-doc:find-tag (class-of list))))
    (progn
      (cl-markup:markup* (cons list-tag (doc->xml (children list))))
      nil)))

(defun emit (node stream)
  "Produce a Plump node from a CommonDoc document."
  (let ((markup:*output-stream* stream))
    (plump:parse (doc->xml node))))
