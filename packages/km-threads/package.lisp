;;; package.lisp
;;; Defines the KM-THREADS package for the km-threads system

;;; Ensure this code runs at compile, load, and execute time
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Create the KM-THREADS package if it doesn't exist
  (unless (find-package :km-threads)
    (make-package :km-threads
                  :use '(:common-lisp
                         :jsown
                         :bordeaux-threads))))

;;; Set the current package to KM-THREADS
(in-package :km-threads)

;;; Define a variable to track package usage (optional, following KM pattern)
(defvar *using-km-threads-package* nil)
(setq *using-km-threads-package* t) ; Indicate packaged version is in use

;;; Define a constant for the KM-THREADS package
(defconstant *km-threads-package* *package*)