(in-package :decent)

(defvar *next-route*)

(defclass router ()
  ((routes :initform nil :initarg :routes :accessor routes)))

(defclass route ()
  ((method :initarg :method :accessor route-method)
   (regex :initarg :regex :accessor route-regex)
   (handler :initarg :handler :accessor route-handler)))

(defun make-router ()
  (make-instance 'router))

(defvar *router* (make-router))

(defun add-route (router new-route)
  (loop for r on (routes router)
     do
       (when (and (eq (route-method new-route) (route-method (car r)))
		  (string= (route-regex new-route) (route-regex (car r))))
	 (format t "redefining route ~a~%" (route-regex (car r)))
	 (rplaca r new-route)
	 (mapcar (lambda (r) (format t "~a~%" (route-regex r))) (routes router))
	 (return-from add-route new-route)))
  (setf (routes router) (append (routes router) (list new-route))))

(defun make-route (method regex handler)
  (make-instance 'route :regex regex :method method :handler handler))

(defun next-route ()
  (funcall *next-route*))

(defun dispatch (routes method uri)
  (loop for (hd . tl) on routes
     do
       (when (or (eq (route-method hd) method) (eq (route-method hd) :ANY))
	 (multiple-value-bind (match regs) (scan-to-strings (route-regex hd) uri)
	   (when match
	     (labels ((regs->args (regs)
			(if (> (length regs) 0)
			    (map 'list #'identity regs)
			    nil)))
	       (let ((*next-route* (lambda () (dispatch tl method uri))))
		 (let ((response
			(apply (route-handler hd) (regs->args regs))))
		   (return response)))))))))

(defun mk-fun-args (args)
  (if (null args)
      '()
      `(&optional ,@args)))

(defmacro defroute (spec args &body body)
  (typecase spec
    (string
     ;; spec contains just a regex
     (let ((method :GET)
	   (regex (format nil "^~a$" spec)))
       `(add-route *router* (make-route ,method ,regex (lambda ,(mk-fun-args args) ,@body)))))
    (cons
     ;; spec is (method uri)
     (let ((method (car spec))
	   (regex (format nil "^~a$" (cadr spec))))
       `(add-route *router* (make-route ,method ,regex (lambda ,(mk-fun-args args) ,@body)))))))
