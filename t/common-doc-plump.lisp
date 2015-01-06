(in-package :cl-user)
(defpackage common-doc-plump-test
  (:use :cl :fiveam))
(in-package :common-doc-plump-test)

(run! 'common-doc-plump-test.parser:parser)
(run! 'common-doc-plump-test.emitter:emitter)
