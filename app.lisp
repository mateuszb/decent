(in-package :decent)

(defvar *app*)

(defclass app ()
  ((port :initform nil :initarg :port :accessor app-port)
   (scopes :initform nil :initarg :scopes :accessor app-scopes)))

(defclass scope ()
  ((matcher :initform nil :initarg :matchfn :accessor scope-matcher)
   (spec :initform nil :initarg :spec :accessor scope-spec)
   (router :initform nil :initarg :router :accessor scope-router)))

(defmethod print-object ((a app) stream)
  (print-unreadable-object (a stream :type t)
    (format stream "port=~d, scopes=~{~a~^~%~}" (app-port a) (app-scopes a))))

(defmethod print-object ((s scope) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "spec: ~a, ~a" (scope-spec s) (scope-router s))))

(defmacro make-app (port &body scopes)
  `(make-instance 'app :port ,port :scopes (list ,@scopes)))

(defun route-spec->route (route-spec)
  `(make-route ,(car route-spec) ,(cadr route-spec) #',(caddr route-spec)))

(defmacro with-scope (scope-spec &body routes)
  (let ((router (gensym)))
    (multiple-value-bind (route-additions)
	(loop for route-spec in routes
	   collect `(add-route ,router ,(route-spec->route route-spec)))
      `(let ((,router (make-router)))
	 ,@route-additions
	 (make-instance 'scope :matchfn (make-scope-matcher ,scope-spec)
			:spec ',scope-spec
			:router ,router)))))

(defmacro make-scope-matcher (scope-spec)
  (multiple-value-bind (syms vals) 
      (loop for (head . tail) on scope-spec by #'cddr
	 collect (intern (symbol-name head) :keyword) into symbols
	 collect (car tail) into vals
	 finally (return (values symbols vals)))

    `(lambda (request)
       (with-slots (headers) request
	 (and
	  ,@(loop for k in syms
	       for v in vals
	       collect
		 (typecase v
		   (keyword `(eq (gethash ,k headers) ,v))
		   (string
		    `(let ((val (gethash ,k headers "")))
		       (when (>= (length val) (length ,v))
			 (string= (subseq val 0 (length ,v)) ,v))))
		   (symbol
		    (let ((keyword (intern (symbol-name v) :keyword)))
		      `(eq (gethash ,k headers) ,keyword)))
		   (number
		    `(= (parse-integer (gethash ,k headers "0")) ,v)))))))))

(defun app-handler (request)
  (with-slots (scopes) *app*
    (loop for scope in scopes
       with result = nil
       while (null result)
       do
	 (with-slots (router matcher) scope
	   (let ((*router* router))
	     (when (funcall matcher request)
	       (setf result (dispatch (routes router) request))
	       (loop-finish))))
       finally
	 (if result
	     (return result)
	     (return (list 404 (list (cons "Content-Length" 0)) ""))))))

(defun mime-type (fname)
  (trivial-mimes:mime fname))
