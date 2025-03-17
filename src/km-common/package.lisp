#|
package:
- Author: scott
- Date: 2025-02-28
|#
;;; packages\km-common\package.lisp
(defpackage #:km-common
  (:use #:cl)
  (:export
   #:make-knowledge-base      ; Example: Create a basic knowledge structure
   #:add-fact                 ; Add a fact to the knowledge base
   #:query-fact               ; Query a fact
   #:log-message))            ; Simple logging utility