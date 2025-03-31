(defpackage :km-rest
  (:use :cl :hunchentoot :quux-hunchentoot :jsown :km :km-logging)
  (:export :start-server :stop-server))

(in-package :km-rest)

(defvar *server* nil "Holds the server instance.")
(defvar *stop-requested* nil "Flag for shutdown request.")
(defvar *shutdown-thread* nil "Thread for shutdown handling.")
(defvar *thread-pool* nil "Holds the thread pool instance.")
(defvar *default-port* 8080 "Default port if unspecified.")

(km-logging:setup-file-logging "km-service.log" :info)

(defparameter *banned-functions* '() "Banned KM functions.")
(defparameter *banned-symbols* '() "Banned KM symbols.")

(defun get-number-of-cores ()
  "Return the number of available CPU cores."
  (cond
    ((uiop:os-windows-p)
     (let ((num-str (uiop:getenv "NUMBER_OF_PROCESSORS")))
       (if num-str
           (parse-integer num-str :junk-allowed t)
           1)))
    ((uiop:os-unix-p)
     (handler-case
         (parse-integer (string-trim '(#\Newline) (uiop:run-program "nproc" :output :string)))
       (error () 1)))
    (t 1)))

(defun validate-expression (expr-str)
  "Convert a KM expression string to a list, preserving case and interning symbols in the KM package."
  (handler-case
      (let ((*readtable* (copy-readtable nil))
            (*package* (find-package :km)))
        (setf (readtable-case *readtable*) :preserve)
        (let ((expr (read-from-string expr-str)))
          (unless (listp expr) (error "Expression must be a list"))
          (format t "VALIDATE-EXPRESSION-1: ~A~%" expr)
          (when (and (symbolp (first expr))
                     (member (symbol-name (first expr)) *banned-functions* :test #'string-equal))
            (error "Disallowed function: ~a" (first expr)))
          (dolist (arg (rest expr))
            (when (and (symbolp arg)
                       (member (symbol-name arg) *banned-symbols* :test #'string-equal))
              (error "Disallowed symbol: ~a" arg)))
          expr))
    (error (e) (error "Invalid expression: ~a" e))))

(defun define-handlers ()
  "Define REST endpoints for /km, /km-unique, and /stop."
  (define-easy-handler (km-handler :uri "/km" :default-request-type :post) ()
    "Handle POST requests to /km, evaluating KM expressions directly."
    (setf (content-type*) "application/json")
    (format t "Entered define-handlers.~%")
    (handler-case
      (let* ((data (jsown:parse (raw-post-data :force-text t)))
            (expr-str (if (and (jsown:keyp data "expr") (stringp (jsown:val data "expr")))
                          (jsown:val data "expr")
                          (error "Missing or invalid 'expr' field")))
            (fail-mode (if (string-equal (or (and (jsown:keyp data "fail_mode") 
                                                  (jsown:val data "fail_mode")) 
                                              "fail") 
                                          "error") 
                            'error 'fail))
            (expr (validate-expression expr-str)))
          (unless (member fail-mode '(fail error)) (error "Invalid 'fail_mode'"))
          (format t "KM-EXPRESSION: ~A~%" expr-str)
          (handler-case
              (let ((result (km:km expr :fail-mode fail-mode)))
                (format t "KM-RESULT: ~A" result)
                (jsown:to-json (mapcar #'prin1-to-string result)))
            (error (e)
              (log:error "KM error: ~a" e)
              (jsown:to-json (jsown:new-js ("error" (format nil "~a" e)))))))
      (error (e)
        (setf (return-code*) +http-bad-request+)
        (jsown:to-json (jsown:new-js ("error" (format nil "~a" e)))))))

  (define-easy-handler (km-unique-handler :uri "/km-unique" :default-request-type :post) ()
    "Handle POST requests to /km-unique, evaluating KM expressions directly."
    (setf (content-type*) "application/json")
    (handler-case
        (let* ((data (jsown:parse (raw-post-data :force-text t)))
               (expr-str (if (and (jsown:keyp data "expr") (stringp (jsown:val data "expr")))
                             (jsown:val data "expr")
                             (error "Missing or invalid 'expr' field")))
               (fail-mode (if (string-equal (jsown:val-safe data "fail_mode" "fail") "error") 'error 'fail))
               (expr (validate-expression expr-str)))
          (unless (member fail-mode '(fail error)) (error "Invalid 'fail_mode'"))
          (format t "KM-EXPRESSION: ~A" expr-str)
          (handler-case
              (let ((result (km:km-unique expr :fail-mode fail-mode)))
                (format t "KM-RESULT: ~A" result)
                (jsown:to-json (jsown:new-js ("value" (prin1-to-string result)))))
            (error (e)
              (log:error "KM error: ~a" e)
              (jsown:to-json (jsown:new-js ("error" (format nil "~a" e)))))))
      (error (e)
        (setf (return-code*) +http-bad-request+)
        (jsown:to-json (jsown:new-js ("error" (format nil "~a" e)))))))

  (define-easy-handler (stop-handler :uri "/stop" :default-request-type :get) ()
    "Stop the KM REST server."
    (setf (content-type*) "application/json")
    (stop-server)
    (setf *stop-requested* t)
    (jsown:to-json (jsown:new-js ("status" "stopped")))))

(defun start-server (&optional (port *default-port*))
  "Start the KM REST server on the specified port, limiting connections to the number of CPU cores."
  (when *server*
    (hunchentoot:stop *server*)
    (format t "Stopped existing server~%"))
  (let* ((num-cores (get-number-of-cores))
         (taskmaster (make-instance 'quux-hunchentoot:thread-pooling-taskmaster
                                    :max-thread-count num-cores))
         (*server* (make-instance 'hunchentoot:easy-acceptor
                                  :port port
                                  :taskmaster taskmaster)))
    (define-handlers)
    (hunchentoot:start *server*)
    (format t "Server started on port ~a with ~a worker threads~%" port num-cores)))

(defun stop-server ()
  "Stop the KM REST server."
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil)))