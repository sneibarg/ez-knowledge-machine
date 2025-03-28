;;; package.lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :km)
    (defpackage :km
      (:use :common-lisp)
      (:export :km :km-unique0))))  ; Export the "KM" symbol

(in-package :km)
(defvar *using-km-package* nil)
(setq *using-km-package* t)
(defconstant *km-package* *package*)
#+mcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'ccl:neq))