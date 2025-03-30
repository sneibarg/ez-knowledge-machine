(defpackage :km-logging
  (:use :common-lisp :log4cl)
  (:export :setup-file-logging))

(in-package :km-logging)

(defun setup-file-logging (filename &optional (level :info))
  "Set up the root logger to log messages to the specified file with the given log level.
   FILENAME is the path to the log file.
   LEVEL can be a keyword such as :debug, :info (default), :warn, or :error."
  (let ((appender (make-instance 'log4cl:file-appender :file filename)))
    (log4cl:add-appender log4cl:*root-logger* appender)
    (log4cl:set-log-level log4cl:*root-logger* level)))