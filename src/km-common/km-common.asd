;;; -*- mode: Lisp; Base: 10 ; Syntax: ANSI-Common-Lisp ; Package: CL-USER ; buffer-read-only: t; -*-
;;; Author: scott
;;; Date: 2025-02-28
(asdf:defsystem #:km-common
  :components ((:file "package")
               (:file "common" :depends-on ("package"))))