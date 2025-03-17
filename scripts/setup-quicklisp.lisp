(require 'asdf)

(defun download-quicklisp ()
  "Download quicklisp.lisp if it doesn't exist in the current directory."
  (unless (probe-file "quicklisp.lisp")
    (format t "Downloading quicklisp.lisp...~%")
    (uiop:run-program "curl -O https://beta.quicklisp.org/quicklisp.lisp")
    (format t "Download complete.~%")))

(defun setup-quicklisp (&key path (add-to-init-file t) initial-libraries &allow-other-keys)
  "Set up a new Quicklisp environment with customizable options.

  Keyword arguments:
    :path            - Installation directory (optional; defaults to Quicklisp's default).
    :add-to-init-file - If t (default), add Quicklisp to the Lisp init file; if nil, skip.
    :initial-libraries - List of library names to install and load after setup (optional).
    Other keywords    - Ignored, allowing unnecessary parameters without errors."
  (download-quicklisp)
  (format t "Loading quicklisp.lisp...~%")
  (load "quicklisp.lisp")
  (format t "Installing Quicklisp...~%")
  (if path
      (quicklisp-quickstart:install :path path)
      (quicklisp-quickstart:install))
  (format t "Quicklisp installed.~%")
  (when add-to-init-file
    (format t "Adding Quicklisp to init file...~%")
    (ql:add-to-init-file))
  (dolist (lib initial-libraries)
    (format t "Installing and loading library ~a...~%" lib)
    (ql:quickload lib))
  (format t "Setup complete.~%"))