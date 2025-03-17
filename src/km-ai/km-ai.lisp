#|
km-ai:
- Author: scott
- Date: 2025-02-28
|#
;;; packages\km-ai\package.lisp
(defpackage #:km-ai
  (:use #:cl #:km-common #:km-utils #:km-core)
  (:export
   #:train-model              ; Train an AI model on KM data
   #:predict-outcome          ; Predict based on knowledge
   #:analyze-knowledge        ; Analyze patterns in KM data
   #:optimize-rules))         ; Optimize KM rules using AI