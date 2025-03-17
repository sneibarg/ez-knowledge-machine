
(unless (find-package :km) (make-package :km :use '(:common-lisp)))
(in-package :km)

;;; File: compiler.lisp
;;; Author: Adam Farquhar (afarquhar@slb.com)
;;; Purpose: Partially flatten the code for the KM dispatch mechanism, which
;;;	in limited tests gives a 10%-30% speed-up in execution speed.

;;; Many thanks to Adam Farquhar for this neat bit of coding!!

(defun reuse-cons (a b ab)
  (if (and (eql a (car ab))
	   (eql b (cdr ab)))
      ab
      (cons a b)))

(defun variables-in (x)
  (let ((vars nil))
    (labels ((vars-in (x)
	       (cond
		 ((consp x)
		  (vars-in (first x))
		  (vars-in (rest x)))
		 ((var-p x)
		  (pushnew x vars))
		 ((eql x '&rest)
		  (pushnew 'rest vars)))))
      (vars-in x)
      (nreverse vars))))


(defun args-to-symbol (&rest args)
  (intern (string-upcase (format nil "~{~a~}" args)) *km-package*))

(defun add-quote-if-needed (x)
  "Quote X if necessary."
  (if (or (numberp x)
	  (stringp x)
	  (and (consp x) (eql (first x) 'quote))
	  (keywordp x))
      x
      (list 'quote x)))

;; See Norvig pg. 180ff for description of Delay, Force.

(defstruct delay (value nil)(function nil))
(defmacro delay (&rest body)
  `(make-delay :function #'(lambda () . ,body)))
(defun force (x)
  (if (not (delay-p x))
      x
      (progn
	(when (delay-function x)
	  (setf (delay-value x)
		(funcall (delay-function x)))
	  (setf (delay-function x) nil)
	  (delay-value x)))))

;;; Rule Compiler
;;;

(defvar *bindings* nil
  "Alist (pattern-var . binding), used for rule compilation.")


(defun compile-rule (pattern consequent var)
  (let ((*bindings* nil))
    `(lambda (,var)
      ,(compile-expr var pattern consequent))))

(defun compile-rules (rules var)
  "A rules is of the form (pat code) where code may reference vars in pat."
  (reduce
   #'merge-code
   (loop for (pattern consequent) in rules
	 collect (compile-rule pattern consequent var))))

(defun compile-expr (var pattern consequent)
  (cond
    ((assoc pattern *bindings* :test #'eq)
     `(when (equal ,var ,(cdr (assoc pattern *bindings*)))
       ,(force consequent)))
    ((var-p pattern)
     (push (cons pattern var) *bindings*)
     ;; `(let ((,pattern ,var)) ,(force consequent))
     ;; do nothing, the consequent needs to get the bindings and use
     ;; it!
     (force consequent)
     )
    ((atom pattern)
     `(when (eql ,var ,(add-quote-if-needed pattern))
       ,(force consequent)))
    (t
     (compile-list var pattern consequent)
     )))

(defun compile-list (var pattern consequent)
  (let ((L (args-to-symbol var 'l))
	(r (args-to-symbol var 'r)))
    (if (consp pattern)
	(if (equal pattern '(&rest))
	    (progn
	      ;;(push (cons 'rest `(list ,var)) *bindings*)
	      (push (cons 'rest var) *bindings*)
	      (force consequent))
	    `(when (consp ,var)
	      (let ((,L (first ,var))
		    (,R (rest  ,var)))
		,(compile-expr
		  L (first pattern)
		  (delay (compile-expr R (rest pattern) consequent))))))
      `(when (null (cdr ,var))
	     (let ((,L (first ,var)))
	       ,(compile-expr
		 L (first pattern) consequent))))))

(defun mergeable (a b)
  ;; (f x y) (f x z) => (f x (merge y z))
  ;; also handles our when, let (only one element in body)
  (and (listp a) (listp b)
       (= (length a) (length b) 3)
       (equal (first a) (first b))
       (equal (second a) (second b))))

(defun merge-code (a b)
  ;; A and B are pieces of code generated by the pattern
  ;; compiler. Merge them (disjunctively) together.
  (cond
     ((mergeable a b)
      ;; (f x y) (f x z) => (f x (merge y z))
      ;; also handles our when, let (only one element in body)
       (list (first a)
	     (second a)
	     (merge-code (third a) (third b))))

     ((and (consp a) (eql 'or (first a)))
      ;; want to try to merge in with some interesting disjunct if
      ;; possible
      (let ((pos (position-if #'(lambda (x) (mergeable b x)) a)))
	(cond
	  ((null pos)
	   ;; just add b as a disjunct
	   (if (and (consp b) (eql 'or (first b)))
	       `(or ,@(rest a) ,@(rest b))
	       `(or ,@(rest a) ,b)))
	  (t
	   ;; merge b with one of a's disjuncts
	   `(,@(subseq a 0 pos)
	     ,(merge-code (nth pos a) b)
	     ,@(subseq a (1+ pos)))))))
     (t
      `(or ,a ,b))))

;;;
;;; KM Handler compilation
;;;

#|
#+ignore(defun dereference-expr (x)
  ;; note depending on the compiler, this can be slow.
  (if (consp x)
      (reuse-cons
       (dereference-expr (first x))
       (dereference-expr (rest  x))
       x)
      (dereference x)))
|#
#|
;;; Move to interpreter lisp
(defun dereference-expr (x)
  ;; This is fundamentally WRONG, but is the existing 1.2 behavior.
  (if (consp x)
      (mapcar #'dereference x)
      (dereference x)))
|#

; (defparameter *km-handler-function* nil) - now in header.lisp
; no more (defparameter *custom-km-handler-function* nil)

(defun reset-handler-functions ()
  (format t "Compiling KM dispatch mechanism...")
  (setq *km-handler-function*
	(compile-handlers *km-handler-alist*))
  (format t "done!~%"))
; no more  (setq *custom-km-handler-function*
; no more	(compile-handlers *custom-km-handlers*)))

(defparameter *trace-rules* nil)

(defun trace-rule (rule-pattern fact bindings)
  (format *trace-output*
   "Rule ~s is being applied to ~s with bindings ~s."
    rule-pattern fact bindings))

(defun compile-handlers (handlers &key code-only)
  "Compile the handler-alist Handlers.  If code-only is T, then just
return the code without invoking the compiler on it."
  (if (null handlers)
      (if code-only
          nil
          #'(lambda (fmode target X) (declare (ignore fmode target X)) nil))
      (let ((code
             (reduce
              #'merge-code
              (loop
               for (pattern closure)
               in handlers
               collect
               `(lambda (f-mode target x)
		 (block km-handler .
			,(cddr
			  (compile-rule
			   pattern
			   (delay

; OLD			    `(let ()
;			       (when *trace-rules*
;				(trace-rule ',pattern X (list ,@(bindings-for pattern))))
;			      (return-from km-handler
;				(funcall
;				 ',closure f-mode
;				 ,@(bindings-for pattern)))))

#|NEW|#			   `(return-from km-handler
			       (values
				(funcall
				 #',closure f-mode target
;				 #',closure f-mode
				 ,@(bindings-for pattern))
				',pattern)))

			   'x))))))))
        (if code-only
            code (compile nil code)))))

(defun bindings-for (pattern)
   (loop for var in (variables-in pattern)
	 collect (cdr (assoc var *bindings*))))

#|
;;; AUX FUNCTIONS FROM KM SOURCE
;;; This is defined in km.lisp already. Need this for stand-alone compiler.
(defun var-p (var)
  (and (symbolp var)
       (char=
       #\?
       (char (the string (symbol-name (the symbol var))) 0))))
|#

(defparameter *compiled-handlers-file* "compiled-handlers.lisp")

;;; [1] Note, don't make this universal, as we lose debugging info (which users would like). Lispworks has constraints which
;;; make the full compilation without this setting a problem.
(defun write-compiled-handlers ()
  (let* ( (anonymous-function (compile-handlers *km-handler-alist* :code-only t))
          (named-function `(defun compiled-km-handler-function (f-mode target x)
;;; Need to add this manually to compiled-handlers (it gets stripped off here)
;;;                 #+harlequin-pc-lisp (declare (optimize (debug 0)))     ; patch for Lispworks from Francis Leboutte [1]
                             .
			     ,(rest (rest anonymous-function))))		   ; strip off "(lambda (f-mode x) ..."
	  (stream (tell *compiled-handlers-file*)) )
    (format stream "
;;; File: compiled-handlers.lisp
;;; Author: MACHINE GENERATED FILE, generated by compiler.lisp (author Adam Farquahar)
;;; This file was generated by (write-compiled-handlers) in compiler.lisp.
;;; This partially flattens the code assigned to *km-handler-list*, which results in
;;; 10%-30% faster execution (10%-30%) at run-time. Loading of this file is optional,
;;; KM will be slower if it's not loaded. For the legible, unflattened source of this
;;; flattened code, see the file interpreter.lisp.
;;;
;;; NOTE: manually insert the line after compiled-km-handler-function for KM release:
;;;
;;; (defun compiled-km-handler-function (f-mode x)
;;;   #+harlequin-pc-lisp (declare (optimize (debug 0)))     ; patch for Lispworks from Francis Leboutte [1]
;;;  (block km-handler
;;;	...
;;;
;;; ==================== START OF MACHINE-GENERATED FILE ====================

(setq *compile-handlers* t)

")
   (write named-function :stream stream)
   (format stream "

(setq *km-handler-function* #'compiled-km-handler-function)

;;; This file was generated by (write-compiled-handlers) in compiler.lisp.
;;; This partially flattens the code assigned to *km-handler-list*, which results in
;;; 10%-30% faster execution (10%-30%) at run-time. Loading of this file is optional,
;;; KM will be slower if it's not loaded. For the legible, unflattened source of this
;;; flattened code, see the file interpreter.lisp.

;;; ==================== END OF MACHINE-GENERATED FILE ====================

")
   (close stream)
   (format t "Compiled handlers written to the file ~a~%" *compiled-handlers-file*)))


