(defpackage :km-rest
  (:use :cl :hunchentoot :jsown :km :km-threads)
  (:export :start-server :stop-server :run-in-thread-pool))

(in-package :km-rest)

(defvar *server* nil "Global variable to hold the server instance.")
(defvar *thread-pool* nil "Global variable to hold the thread pool instance.")
(defvar *default-port* 8080 "Default port number if none is specified.")

(defparameter *allowed-functions*
  '("instance-of" "subclass-of" "has-property" "all-instances" "all-subclasses"
    "equal" "not-equal" "greater-than" "less-than" "and" "or" "not"
    "has-value" "is-a" "has-slot" "slot-value")
  "List of allowed KM functions for expression enforcement.")

(defparameter *allowed-symbols*
  '("Person" "Animal" "Vehicle" "Dog" "Cat" "Car" "Truck" "John" "Jane" "Fido" "Whiskers"
    "has-age" "has-name" "has-owner" "has-color" "has-weight" "has-speed"
    "true" "false" "nil")
  "List of allowed KM symbols for expression enforcement.")

(defun validate-expression (expr-str)
  "Validate a KM expression string to ensure it only uses allowed functions and symbols."
  (handler-case
      (let ((expr (read-from-string expr-str)))
        (unless (listp expr)
          (error "Expression must be a list"))
        (let ((function-name (first expr)))
          (unless (and (symbolp function-name)
                       (member (symbol-name function-name) *allowed-functions* :test #'string-equal))
            (error "Disallowed function: ~a" function-name))
          (dolist (arg (rest expr))
            (unless (or (stringp arg)
                        (numberp arg)
                        (and (symbolp arg)
                             (member (symbol-name arg) *allowed-symbols* :test #'string-equal)))
              (error "Disallowed symbol or argument: ~a" arg)))))
    (error (e)
      (error "Invalid expression: ~a" e))))

(defun run-in-thread-pool (task-fn)
  "Submit TASK-FN to the global thread pool and return its result."
  (run-task *thread-pool* task-fn))

(defun define-handlers ()
  "Define the REST endpoint handlers for /km and /km-unique."
  (define-easy-handler (km-handler :uri "/km" :default-request-type :post) ()
    "Handle POST requests to /km, evaluating the KM expression using the thread pool."
    (setf (content-type*) "application/json")
    (handler-case
        (let* ((json (raw-post-data :force-text t))
               (data (parse json)))
          (unless (and (jsown:keyp data "expr") (stringp (jsown:val data "expr")))
            (error "Missing or invalid 'expr' field; must be a string"))
          (let ((expr-str (jsown:val data "expr"))
                (fail-mode-str (if (jsown:keyp data "fail_mode")
                                 (jsown:val data "fail_mode")
                                 "fail")))
            (unless (member fail-mode-str '("fail" "error") :test #'string-equal)
              (error "Invalid 'fail_mode'; must be 'fail' or 'error'"))
            (validate-expression expr-str)
            (let ((fail-mode (if (string-equal fail-mode-str "error") 'error 'fail))
                  (expr (read-from-string expr-str)))
              (let ((result (run-in-thread-pool (lambda () (km:km expr :fail-mode fail-mode)))))
                (encode-json-to-string (mapcar #'prin1-to-string result))))))
      (error (e)
        (setf (return-code*) +http-bad-request+)
        (encode-json-to-string (list :error (format nil "~a" e))))))

  (define-easy-handler (km-unique-handler :uri "/km-unique" :default-request-type :post) ()
    "Handle POST requests to /km-unique, evaluating the KM expression using the thread pool."
    (setf (content-type*) "application/json")
    (handler-case
        (let* ((json (raw-post-data :force-text t))
               (data (parse json)))
          (unless (and (jsown:keyp data "expr") (stringp (jsown:val data "expr")))
            (error "Missing or invalid 'expr' field; must be a string"))
          (let ((expr-str (jsown:val data "expr"))
                (fail-mode-str (if (jsown:keyp data "fail_mode")
                                 (jsown:val data "fail_mode")
                                 "fail")))
            (unless (member fail-mode-str '("fail" "error") :test #'string-equal)
              (error "Invalid 'fail_mode'; must be 'fail' or 'error'"))
            (validate-expression expr-str)
            (let ((fail-mode (if (string-equal fail-mode-str "error") 'error 'fail))
                  (expr (read-from-string expr-str)))
              (let ((result (run-in-thread-pool (lambda () (km:km-unique0 expr :fail-mode fail-mode)))))
                (encode-json-to-string (list :value (prin1-to-string result)))))))
      (error (e)
        (setf (return-code*) +http-bad-request+)
        (encode-json-to-string (list :error (format nil "~a" e)))))))

(defun start-server (&optional (port *default-port*))
  (when *server*
    (format t "Stopping existing server~%")
    (stop-server))
  (format t "Initializing thread pool~%")
  (setf *thread-pool* (make-thread-pool :name "km-rest-pool"))
  (format t "Thread pool initialized~%")
  (setf *server* (make-instance 'hunchentoot:easy-acceptor :port port))
  (format t "Acceptor created~%")
  (define-handlers)
  (format t "Handlers defined~%")
  (hunchentoot:start *server*)
  (format t "KM REST server started on port ~a~%" port))

(defun stop-server ()
  (format t "Entering stop-server~%")
  (when *server*
    (format t "Stopping server: ~a~%" *server*)
    (hunchentoot:stop *server*)
    (setf *server* nil)
    (format t "Server stopped~%"))
  (when *thread-pool*
    (format t "Shutting down thread pool~%")
    (shutdown-thread-pool *thread-pool*)
    (setf *thread-pool* nil)
    (format t "Thread pool shut down~%"))
  (format t "Exiting stop-server~%"))