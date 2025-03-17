;; scripts\check-deps.lisp
(load #p"E:/IdeaProjects/ez-knowledge-machine/IdeaProjectsez-knowledge-machinequicklisp/setup.lisp")
(ql:quickload :asdf)
(pushnew #p"E:/IdeaProjects/ez-knowledge-machine/IdeaProjectsez-knowledge-machinequicklisp/local-projects/" asdf:*central-registry* :test #'equal)
(ql:register-local-projects)
(format t "Quicklisp Local Project Directories: ~A~%" ql:*local-project-directories*)
(format t "Registered Local Projects: ~A~%" (ql:local-projects))
(format t "ASDF Registry: ~A~%" asdf:*central-registry*)
(dolist (system '(:km-common :km-utils :km-core :km-ai))
  (handler-case
      (progn
        (asdf:load-system system)
        (format t "~A loaded successfully.~%" system))
    (error (e)
      (format t "Error loading ~A: ~A~%" system e))))