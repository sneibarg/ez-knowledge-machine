#|
common:
- Author: scott
- Date: 2025-02-28
|#
;;; packages\km-common\common.lisp
(in-package #:km-common)

;; Simple knowledge base structure (example)
(defstruct knowledge-base
  facts)

(defun make-knowledge-base ()
  "Create an empty knowledge base."
  (make-instance 'knowledge-base :facts nil))

(defun add-fact (kb fact)
  "Add a fact to the knowledge base."
  (push fact (knowledge-base-facts kb)))

(defun query-fact (kb fact)
  "Check if a fact exists in the knowledge base."
  (member fact (knowledge-base-facts kb)))

(defun log-message (message)
  "Log a message to standard output."
  (format t "LOG: ~A~%" message))