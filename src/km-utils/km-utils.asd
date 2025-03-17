;;; -*- mode: Lisp; Base: 10 ; Syntax: ANSI-Common-Lisp ; Package: CL-USER ; buffer-read-only: t; -*-
;;; Author: scott
;;; Date: 2025-02-28

;; packages/km-core/km-utils.asd
(asdf:defsystem #:km-utils
  :description "Utility functions for the Knowledge Machine"
  :depends-on (#:km-common)
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))))