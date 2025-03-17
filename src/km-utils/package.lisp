#|
package:
- Author: scott
- Date: 2025-02-28
|#
;;; packages\km-utils\package.lisp
(defpackage #:km-utils
  (:use #:cl #:km-common)
  (:export
   #:parse-input              ; Parse user or file input
   #:format-knowledge         ; Format knowledge for display
   #:validate-data            ; Validate data integrity
   #:with-logging))           ; Macro for logging operations