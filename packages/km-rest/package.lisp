;;; package.lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :km-rest)
    (defpackage :km-rest
      (:use :common-lisp)
      (:export :start-server :stop-server :define-handlers))))  

(in-package :km-rest)
(defvar *using-km-package* nil)
(setq *using-km-package* t)
(defconstant *km-package* *package*)
#+mcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'ccl:neq))