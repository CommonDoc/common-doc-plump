(in-package :cl-user)
(defpackage common-doc-plump.emitter
  (:use :cl))
(in-package :common-doc-plump.emitter)

(defgeneric emit (node)
  (:documentation "Produce a Plump node from a CommonDoc document."))
