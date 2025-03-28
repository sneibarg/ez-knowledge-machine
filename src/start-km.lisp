;;; start-km.lisp
(let ((quicklisp-setup "E:/Quicklisp/setup.lisp"))
  (unless (find-package :ql)
    (if (probe-file quicklisp-setup)
        (load quicklisp-setup)
        (error "Quicklisp setup file not found at ~A. Please install Quicklisp first." quicklisp-setup))))

;;; Display command-line arguments for debugging
(format t "ARGV: ~S~%" sb-ext:*posix-argv*)

;;; Define parameters with defaults and types
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
    (*built-in-remove-subsumers-slots* . (:default "#$(instance-of classes superclasses member-type)" :type string))
    (*built-in-remove-subsumees-slots* . (:default "#$(subclasses prototype-of domain range)" :type string))
    ;;; (*default-fluent-status* . (:default "#$*Fluent" :type symbol))
    (*active-obj-stack* . (:default "nil" :type list))
    ;;; (*on-error* . (:default "debug" :type symbol))
    (*justify-leaves* . (:default "nil" :type boolean))
    (*start-justifications-with-because* . (:default "t" :type boolean))
    (*classification-enabled* . (:default "t" :type boolean))
    (*prototype-classification-enabled* . (:default "t" :type boolean))
    (*use-inheritance* . (:default "t" :type boolean))
    (*use-prototypes* . (:default "t" :type boolean))
    (*developer-mode* . (:default "nil" :type boolean))
    (*unclonable-slots* . (:default "#$(prototype-participant-of prototype-participants prototype-of prototypes prototype-scope locked-instance-of has-clones has-built-clones)" :type string))
    (*called-forces-unification* . (:default "t" :type boolean))
    (*port* . (:default "8080" :type integer)))
  "List of KM parameters with their default values and types.")

;;; Load dependencies using Quicklisp
(dolist (dep '(:hunchentoot :jsown :bordeaux-threads :km :km-threads :km-rest))
  (ql:quickload dep))

;;; List of parameter symbols
(defparameter *km-behavior-parameter-symbols*
  (mapcar #'car *km-parameter-info*))

;;; Convert string to Lisp value based on type
(defun string-to-value (str type)
  (cond
    ((eq type 'string) str)
    ((eq type 'boolean) (if (string= str "t") t nil))
    ((eq type 'integer) (parse-integer str))
    ((eq type 'float) (read-from-string str))
    ((eq type 'symbol) (intern (string-upcase str)))
    ((eq type 'list) (read-from-string str))
    ((equal type '(or null integer))
     (if (string= str "nil") nil (parse-integer str)))
    (t (error "Unsupported type: ~A" type))))

;;; Define all parameters with their default values
(dolist (param-info *km-parameter-info*)
  (let* ((sym (car param-info))
         (default-str (getf (cdr param-info) :default))
         (type (getf (cdr param-info) :type))
         (default-value (string-to-value default-str type)))
    (eval `(defparameter ,sym ,default-value))))

;;; Extract arguments after "--"
(defun get-script-arguments ()
  (let* ((argv sb-ext:*posix-argv*)
         (separator-pos (position "--" argv :test #'string=)))
    (if separator-pos
        (subseq argv (1+ separator-pos))
        nil)))

;;; Display usage information
(defun usage ()
  (format t "~&=== KM REST Service Usage ===~%")
  (format t "Run the KM REST service with customizable behavior parameters:~%")
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
  (format t "  sbcl --load start-km.lisp -- --port 9090 --logging t~%")
  (format t "  sbcl --load start-km.lisp -- --max-padding-instances 10 --recursive-classification nil~%")
  (format t "~%Notes:~%")
  (format t "  - Values must match the expected type (e.g., t/nil for boolean, integers, floats).~%")
  (format t "  - Use Lisp syntax for lists and symbols.~%")
  (sb-ext:exit))

;;; Set parameter with type checking
(defun set-parameter (sym value-str)
  (let* ((info (assoc sym *km-parameter-info*))
         (type (getf info :type))
         (value (string-to-value value-str type)))
    (if (typep value type)
        (progn
          (setf (symbol-value sym) value)
          (format t "Set ~A to ~A~%" sym value))
        (error "Invalid value '~A' for parameter ~A (expected type: ~A)" value-str sym type))))

;;; Start the REST server
(defun start ()
  "Start the KM REST server with configured parameters."
  (format t "Calling km-rest:start-server *port*~%")
  ;;; this is our stack overflow culprit: 
  ;;; (setf *thread-pool* (km-threads:make-thread-pool))
  (km-rest:define-handlers)
  (setf *server* (make-instance 'hunchentoot:easy-acceptor :port 8080))
  (hunchentoot:start *server*)
  ;;;(km-rest:start-server *port*)
  (format t "Exiting start.~%"))

;;; Stop the REST server
(defun stop ()
  "Stop the KM REST server."
  (format t "Exiting the km-rest service.~%")
  (km-rest:stop-server))

;;; Main function to process arguments and start server
(defun main ()
  (let ((program-args (get-script-arguments)))
    (cond
      ((null program-args)
       (format t "No arguments provided, starting server with defaults on port ~A~%" *port*)
       (start))
      ((member "--help" program-args :test #'string=)
       (usage))
      (t
       (loop for i from 0 below (length program-args) by 2
             do (let* ((param (nth i program-args))
                       (value (nth (1+ i) program-args))
                       (param-name (subseq param 2)) ; Strip "--"
                       (sym (find (intern (concatenate 'string "*" (string-upcase param-name) "*"))
                                  *km-behavior-parameter-symbols*
                                  :key #'symbol-name
                                  :test #'string=)))
                  (if sym
                      (set-parameter sym value)
                      (error "Unknown parameter: ~A" param-name))))
       (start))))
  ;; Keep process running
  #+sbcl (sb-impl::toplevel-repl nil))

(main)