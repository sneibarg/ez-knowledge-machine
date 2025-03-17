;;; -*- mode: Lisp; Base: 10 ; Syntax: ANSI-Common-Lisp ; Package: CL-USER ; buffer-read-only: t; -*-
;;; Author: scott
;;; Date: 2025-02-28
(asdf:defsystem #:km-ai
  :description "AI components for the Knowledge Machine"
  :depends-on (#:km-common #:km-utils #:km-core)
  :components ((:file "package")
               (:file "ai" :depends-on ("package"))))