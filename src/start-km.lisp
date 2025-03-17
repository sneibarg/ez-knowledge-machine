(format t "ARGV: ~S~%" sb-ext:*posix-argv*)

;;; Define the list of KM behavior parameter symbols
(defparameter *km-behavior-parameter-symbols*
  '(*load-default-components*  ; Added this parameter
    *recursive-classification*
    *indirect-classification*
    *recursive-prototypes*
    *eagerly-unify-prototypes*
    *sanity-checks*
    *slot-checking-enabled*
    *logging*
    *max-padding-instances*
    *tolerance*
    *output-precision*
    *instance-of-is-fluent*
    *km-depth-limit*
    *linear-paths*
    *project-cached-values-only*
    *record-explanations-for-clones*
    *coerce-undeclared-slots*
    *record-explanations*
    *record-sources*
    *add-comments-to-names*
    *check-kb*
    *classify-slotless-instances*
    *built-in-remove-subsumers-slots*
    *built-in-remove-subsumees-slots*
    *default-fluent-status*
    *active-obj-stack*
    *on-error*
    *justify-leaves*
    *start-justifications-with-because*
    *classification-enabled*
    *prototype-classification-enabled*
    *use-inheritance*
    *use-prototypes*
    *developer-mode*
    *unclonable-slots*
    *called-forces-unification*))

(defun find-script-pos (argv)
  (position-if (lambda (arg)
                 (let ((path (ignore-errors (pathname arg))))
                   (and path
                        (string= (pathname-name path) "start-km")
                        (string= (pathname-type path) "lisp"))))
               argv))

(defun get-script-arguments ()
  (let* ((argv sb-ext:*posix-argv*)
         (separator-pos (position "--" argv :test #'string=)))
    (if separator-pos
        (subseq argv (1+ separator-pos))
        (error "No '--' separator found in arguments"))))


(defun usage ()
  ;; Existing code to print usage header...
  (format t "=== KM Interpreter Usage ===~%")
  (format t "Run the KM interpreter with customizable behavior parameters:~%")
  (format t "  sbcl --load start-km.lisp [options]~%~%")
  (format t "Available Options:~%")
  (format t "  --help                Display this help message and exit~%")
  (dolist (sym *km-behavior-parameter-symbols*)
    (let ((name (string-downcase (subseq (symbol-name sym) 1 (1- (length (symbol-name sym)))))))
      (format t "  --~30A Set ~A to a specified value~%" name (symbol-name sym))))
  (format t "=======================~%")
  (sb-ext:exit))

;; Main logic
(let ((args (get-script-arguments)))
  (if (member "--help" args :test #'string=)
      (usage)
      (format t "Arguments: ~a~%" args)))

;;; Load km.lisp and start the KM interpreter
(load "km.lisp")
(km)