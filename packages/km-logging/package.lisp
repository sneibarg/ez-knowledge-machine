(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :km-logging)
    (make-package :km-logging
                  :use '(:common-lisp
                         :log4cl))))
(in-package :km-logging)
(defvar *using-km-logging-package* nil)
(setq *using-km-logging-package* t)
(defconstant *km-logging-package* *package*)