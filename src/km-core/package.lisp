#|
package:
- Author: scott
- Date: 2025-02-28
|#
;;; packages\km-core\package.lisp
(defpackage #:km-core
  (:use #:cl #:km-common #:km-utils)
  (:export
   #:initialize-km            ; Start the Knowledge Machine
   #:process-knowledge        ; Process a set of facts/rules
   #:retrieve-knowledge       ; Retrieve processed knowledge
   #:define-rule))            ; Define a rule in the KM