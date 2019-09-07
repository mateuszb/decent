(in-package :decent)

(defclass router ()
  ((routes :initform nil :initarg :routes :accessor routes)))

(defclass route ()
  ((regex :initarg :regex :accessor route-regex)
   (handler :initarg :handler :accessor route-handler)))

(defun make-router ()
  (make-instance 'router))

(defun add-route (router route)
  (setf (routes router) (append (routes router) (list route))))

(defun make-route (regex handler)
  (make-instance 'route :regex regex :handler handler))

(defvar *next-route*)

(defun next-route ()
  (funcall *next-route*))

(defun dispatch (routes path)
  (loop for (hd . tl) on routes
     do
       (multiple-value-bind (match regs) (scan-to-strings (route-regex hd) path)
	 (when match
	   (let ((*next-route* (lambda () (dispatch tl path))))
	     (return
	       (funcall (route-handler hd))))))))
