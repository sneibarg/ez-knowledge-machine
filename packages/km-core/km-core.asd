;;; -*- mode: Lisp; Base: 10 ; Syntax: ANSI-Common-Lisp ; Package: CL-USER ; buffer-read-only: t; -*-
;;; Author: scott
;;; Date: 2025-02-28
(asdf:defsystem #:km-core
  :description "Core functionality of the Knowledge Machine"
  :depends-on (#:km-common #:km-utils)
  :components ((:file "package")
               (:file "core" :depends-on ("package"))))