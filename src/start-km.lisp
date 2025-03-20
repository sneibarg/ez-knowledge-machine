;;; Display the raw command-line arguments for debugging
(format t "ARGV: ~S~%" sb-ext:*posix-argv*)

;;; Define parameters with their defaults and expected types
(defparameter *km-parameter-info*
  '((*load-default-components* . (:default "t" :type boolean))
    (*recursive-classification* . (:default "t" :type boolean))
    (*indirect-classification* . (:default "t" :type boolean))
    (*recursive-prototypes* . (:default "nil" :type boolean))
    (*eagerly-unify-prototypes* . (:default "t" :type boolean))
    (*sanity-checks* . (:default "nil" :type boolean))
    (*slot-checking-enabled* . (:default "nil" :type boolean))
    (*logging* . (:default "nil" :type boolean))
    (*max-padding-instances* . (:default "0" :type integer))
    (*tolerance* . (:default "0.0001" :type float))
    (*output-precision* . (:default "3" :type integer))
    (*instance-of-is-fluent* . (:default "nil" :type boolean))
    (*km-depth-limit* . (:default "nil" :type (or null integer)))
    (*linear-paths* . (:default "nil" :type boolean))
    (*project-cached-values-only* . (:default "nil" :type boolean))
    (*record-explanations-for-clones* . (:default "t" :type boolean))
    (*coerce-undeclared-slots* . (:default "nil" :type boolean))
    (*record-explanations* . (:default "t" :type boolean))
    (*record-sources* . (:default "t" :type boolean))
    (*add-comments-to-names* . (:default "t" :type boolean))
    (*check-kb* . (:default "nil" :type boolean))
    (*classify-slotless-instances* . (:default "t" :type boolean))
    (*built-in-remove-subsumers-slots* . (:default "#$(instance-of classes superclasses member-type)" :type list))
    (*built-in-remove-subsumees-slots* . (:default "#$(subclasses prototype-of domain range)" :type list))
    (*default-fluent-status* . (:default "#$*Fluent" :type symbol))
    (*active-obj-stack* . (:default "nil" :type list))
    (*on-error* . (:default "debug" :type symbol))
    (*justify-leaves* . (:default "nil" :type boolean))
    (*start-justifications-with-because* . (:default "t" :type boolean))
    (*classification-enabled* . (:default "t" :type boolean))
    (*prototype-classification-enabled* . (:default "t" :type boolean))
    (*use-inheritance* . (:default "t" :type boolean))
    (*use-prototypes* . (:default "t" :type boolean))
    (*developer-mode* . (:default "nil" :type boolean))
    (*unclonable-slots* . (:default "#$(prototype-participant-of prototype-participants prototype-of prototypes prototype-scope locked-instance-of has-clones has-built-clones)" :type list))
    (*called-forces-unification* . (:default "t" :type boolean))))

;;; List of parameter symbols for easy access
(defparameter *km-behavior-parameter-symbols*
  (mapcar #'car *km-parameter-info*))

;;; Convert a string to a Lisp value based on the expected type
(defun string-to-value (str type)
  (cond
    ((eq type 'boolean) (if (string= str "t") t nil))
    ((eq type 'integer) (parse-integer str))
    ((eq type 'float) (read-from-string str))
    ((eq type 'symbol) (intern (string-upcase str)))
    ((eq type 'list) (read-from-string str))
    ((equal type '(or null integer))
     (if (string= str "nil") nil (parse-integer str)))
    (t (error "Unsupported type: ~A" type))))

;;; Extract arguments following the "--" separator
(defun get-script-arguments ()
  (let* ((argv sb-ext:*posix-argv*)
         (separator-pos (position "--" argv :test #'string=)))
    (if separator-pos
        (subseq argv (1+ separator-pos))
        (error "No '--' separator found in arguments"))))

;;; Display usage information
(defun usage ()
  (format t "~&=== KM Interpreter Usage ===~%")
  (format t "Run the KM interpreter with customizable behavior parameters:~%")
  (format t "  sbcl --load start-km.lisp -- [options]~%~%")
  (format t "Available Options:~%")
  (format t "  --help                Display this help message and exit~%")
  (dolist (sym (sort (copy-list *km-behavior-parameter-symbols*) #'string< :key #'symbol-name))
    (let* ((info (assoc sym *km-parameter-info*))
           (default-value (getf info :default))
           (type (getf info :type))
           (name (string-downcase (subseq (symbol-name sym) 1 (1- (length (symbol-name sym)))))))
      (format t "  --~A~30T Set ~A (default: ~A, type: ~A)~%" name (symbol-name sym) default-value type)))
  (format t "~%Examples:~%")
  (format t "  sbcl --load start-km.lisp -- --recursive-classification nil~%")
  (format t "  sbcl --load start-km.lisp -- --max-padding-instances 10 --logging t~%")
  (format t "~%Notes:~%")
  (format t "  - Values must match the expected type (e.g., t/nil for boolean, integers, floats).~%")
  (format t "  - Use Lisp syntax for lists and symbols.~%")
  (sb-ext:exit))

;;; Set a parameter with type enforcement
(defun set-parameter (sym value-str)
  (let* ((info (assoc sym *km-parameter-info*))
         (type (getf info :type))
         (value (string-to-value value-str type)))
    (if (typep value type)
        (progn
          (setf (symbol-value sym) value)
          (format t "Set ~A to ~A~%" sym value))
        (error "Invalid value '~A' for parameter ~A (expected type: ~A)" value-str sym type))))

;;; Main function to process arguments and start KM
(defun main ()
  (let* ((args sb-ext:*posix-argv*)
         (separator-pos (position "--" args :test #'string=)))
    (if separator-pos
        (let ((program-args (subseq args (1+ separator-pos))))
          ;; Process the arguments after the separator
          (loop for i from 0 below (length program-args) by 2
                do (let* ((param (nth i program-args))
                          (value (nth (1+ i) program-args))
                          (param-name (subseq param 2)) ; Strip the "--" prefix
                          (sym (find (intern (concatenate 'string "*" (string-upcase param-name) "*"))
                                     *km-behavior-parameter-symbols*
                                     :key #'symbol-name
                                     :test #'string=)))
                     (if sym
                         (set-parameter sym value) ; Assuming set-parameter exists
                         (error "Unknown parameter: ~A" param-name)))))
        (error "No separator found in arguments")))
  ;; Load km.lisp and call KM (assuming km.lisp defines KM)
  (load "km.lisp")
  (km))
;;; Execute the main function
(main)