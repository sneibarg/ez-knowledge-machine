(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :km-threads)
    (make-package :km-threads
                  :use '(:common-lisp
                         :jsown
                         :bordeaux-threads))))
(in-package :km-threads)
(defvar *using-km-threads-package* nil)
(setq *using-km-threads-package* t)
(defconstant *km-threads-package* *package*)