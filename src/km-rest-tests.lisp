(let ((quicklisp-setup "E:/Quicklisp/setup.lisp"))
  (unless (find-package :ql)
    (if (probe-file quicklisp-setup)
        (load quicklisp-setup)
        (error "Quicklisp setup file not found at ~A. Please install Quicklisp first." quicklisp-setup))))

(ql:quickload :fiveam)
(ql:quickload :drakma)
(ql:quickload :jsown)
(ql:quickload :km-rest)

(fiveam:def-suite km-rest-suite)
(fiveam:in-suite km-rest-suite)
(ql:quickload :flexi-streams)

(fiveam:def-fixture server-fixture ()
  (unwind-protect
      (progn
        (km-rest:start-server 8081 (make-instance 'hunchentoot:single-threaded-taskmaster))
        (sleep 1)
        (&body))
    (km-rest:stop-server)
    (sleep 1)  ; Wait for shutdown
    (setf km-rest:*server* nil)))  ; Ensure global state is reset

(defun parse-response (response)
  (let ((response-str (if (stringp response)
                          response
                          (flexi-streams:octets-to-string response :external-format :utf-8))))
    (jsown:parse response-str)))

(defun send-km-request (expr)
  (let ((url "http://localhost:8081/km")
        (content (jsown:to-json `(:obj ("expr" . ,expr)))))
    (multiple-value-bind (body status)
        (drakma:http-request url
                             :method :post
                             :content-type "application/json"
                             :content content)
      (let ((parsed (parse-response response)))
        (values parsed status)))))

(fiveam:test test-stop-handler
  (fiveam:with-fixture server-fixture ()
    (multiple-value-bind (response status)
        (drakma:http-request "http://localhost:8081/stop"
                             :method :get
                             :content-type "application/json")
      (fiveam:is (equal 200 status))
      (let ((parsed (parse-response response)))
        (fiveam:is (string= "stopped" (jsown:val parsed "status")))))))

(fiveam:test test-reset-kb
  (fiveam:with-fixture server-fixture ()
    (multiple-value-bind (response status)
        (send-km-request "(reset-kb)")
      (fiveam:is (equal 200 status))
      (fiveam:is (equal '("T") response)))))

(fiveam:test test-define-class
  (fiveam:with-fixture server-fixture ()
    (multiple-value-bind (response status)
        (send-km-request "(every Person has (superclasses (Thing)))")
      (fiveam:is (equal 200 status))
      (fiveam:is (equal '("Person") response)))))

(fiveam:test test-define-instance
  (fiveam:with-fixture server-fixture ()
    (multiple-value-bind (response status)
        (send-km-request "(_John has (instance-of (Person)))")
      (fiveam:is (equal 200 status))
      (fiveam:is (equal '("_John") response)))))

(fiveam:test test-query-instance
  (fiveam:with-fixture server-fixture ()
    (multiple-value-bind (response status)
        (send-km-request "(instance-of Person _John)")
      (fiveam:is (equal 200 status))
      (fiveam:is (equal '("T") response)))))

(fiveam:test test-save-kb
  (fiveam:with-fixture server-fixture ()
    (multiple-value-bind (response status)
        (send-km-request "(save-kb \"test.kb\")")
      (fiveam:is (equal 200 status))
      (fiveam:is (equal '("T") response)))))

(fiveam:test test-reset-again
  (fiveam:with-fixture server-fixture ()
    (multiple-value-bind (response status)
        (send-km-request "(reset-kb)")
      (fiveam:is (equal 200 status))
      (fiveam:is (equal '("T") response)))))

(fiveam:test test-query-after-reset
  (fiveam:with-fixture server-fixture ()
    (multiple-value-bind (response status)
        (send-km-request "(instance-of Person _John)")
      (fiveam:is (equal 200 status))
      (fiveam:is (equal '("NIL") response)))))

(fiveam:test test-load-kb
  (fiveam:with-fixture server-fixture ()
    (multiple-value-bind (response status)
        (send-km-request "(load-kb \"test.kb\")")
      (fiveam:is (equal 200 status))
      (fiveam:is (equal '("T") response)))))

(fiveam:test test-query-after-load
  (fiveam:with-fixture server-fixture ()
    (multiple-value-bind (response status)
        (send-km-request "(instance-of Person _John)")
      (fiveam:is (equal 200 status))
      (fiveam:is (equal '("T") response)))))

(fiveam:test test-invalid-function
  (fiveam:with-fixture server-fixture ()
    (multiple-value-bind (response status)
        (send-km-request "(foo bar)")
      (fiveam:is (equal 400 status))
      (fiveam:is (string= "Disallowed function: FOO" (jsown:val response :error))))))

(fiveam:test test-invalid-symbol
  (fiveam:with-fixture server-fixture ()
    (multiple-value-bind (response status)
        (send-km-request "(instance-of UnknownClass John)")
      (fiveam:is (equal 400 status))
      (fiveam:is (string= "Disallowed symbol or argument: UNKNOWNCLASS" (jsown:val response :error))))))

(fiveam:test test-malformed-expression
  (fiveam:with-fixture server-fixture ()
    (multiple-value-bind (response status)
        (send-km-request "not-a-list")
      (fiveam:is (equal 400 status))
      (fiveam:is (string= "Expression must be a list" (jsown:val response :error))))))

(fiveam:test test-missing-expr
  (fiveam:with-fixture server-fixture ()
    (let ((url "http://localhost:8081/km")
          (content "{}"))
      (multiple-value-bind (body status)
          (drakma:http-request url
                               :method :post
                               :content-type "application/json"
                               :content content)
        (let ((parsed (parse-response response)))
          (fiveam:is (equal 400 status))
          (fiveam:is (string= "Missing or invalid 'expr' field; must be a string" (jsown:val parsed :error))))))))