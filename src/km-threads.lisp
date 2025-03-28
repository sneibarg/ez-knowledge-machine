(defpackage :km-threads
  (:use :cl :bordeaux-threads)
  (:export :submit-task :shutdown-thread-pool))

(in-package :km-threads)

;; Thread pool structure with a custom constructor name
(defstruct (thread-pool (:constructor km-thread-pool))
  (task-queue nil :type list)
  (queue-lock (make-lock) :type lock)
  (queue-cond (make-condition-variable) :type condition-variable)
  (threads nil :type list)
  (shutdown-p nil :type boolean))

(defun make-thread-pool (&optional (num-threads (get-number-of-cores)))
  "Create a thread pool with NUM-THREADS worker threads."
  (let ((pool (km-thread-pool)))
    (setf (thread-pool-threads pool)
          (loop :repeat num-threads
                :collect (make-thread :name "km-worker"
                                      :function #'worker-function
                                      :arguments (list pool)))) pool))

(defun worker-function (pool)
  "Worker function that processes tasks from the thread pool's queue."
  (loop
    (when (thread-pool-shutdown-p pool)
      (return))
    (let (task)
      (with-lock-held ((thread-pool-queue-lock pool))
        (if (thread-pool-task-queue pool)
            (setf task (pop (thread-pool-task-queue pool)))
            (condition-wait (thread-pool-queue-cond pool)
                            (thread-pool-queue-lock pool))))
      (when task
        (handler-case
            (funcall task)
          (error (e)
            (format t "Error in task: ~A~%" e)))))))

(defun submit-task (pool task)
  "Add a TASK (a function) to the thread pool's queue."
  (check-type task function)
  (with-lock-held ((thread-pool-queue-lock pool))
    (if (thread-pool-shutdown-p pool)
        (error "Cannot submit task: thread pool is shutting down")
        (progn
          (push task (thread-pool-task-queue pool))
          (condition-notify (thread-pool-queue-cond pool))))))

(defun shutdown-thread-pool (pool)
  "Gracefully shut down the thread pool."
  (with-lock-held ((thread-pool-queue-lock pool))
    (setf (thread-pool-shutdown-p pool) t)
    (condition-broadcast (thread-pool-queue-cond pool)))
  (dolist (thread (thread-pool-threads pool))
    (join-thread thread)))

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

(defvar *thread-pool* (make-thread-pool))