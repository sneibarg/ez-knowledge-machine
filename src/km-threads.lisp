(defpackage :km-threads
  (:use :cl :cl-threadpool)
  (:export :make-thread-pool :submit-task :task-result :shutdown-thread-pool :get-number-of-cores))

(in-package :km-threads)

(defun make-thread-pool (&optional (num-threads (get-number-of-cores)))
  "Create a thread pool with NUM-THREADS worker threads."
  (cl-threadpool:make-threadpool num-threads))

(defun submit-task (pool task)
  "Submit a TASK to the thread pool and return a future."
  (cl-threadpool:add-job pool task))

(defun task-result (future)
  (job-result future))

(defun shutdown-thread-pool (pool)
  "Gracefully shut down the thread pool."
  (cl-threadpool:stop pool))

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