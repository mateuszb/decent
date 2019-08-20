(in-package :decent)

(defvar *routes* (make-hash-table :test 'equal))

(defclass route (t)
  ((path :type 'string)
   (method :type 'keyword)))

(defstruct http-request
  (peer nil)
  (method nil)
  (uri nil)
  (params nil)
  (proto nil)
  (body nil)
  (body-sofar 0)
  (headers (make-hash-table :test 'eq :synchronized t)))

(defun process-request (req)
  (with-slots (peer method uri headers) req
    (multiple-value-bind (route existsp) (gethash uri *routes*)
      ;(format t "~a,~a,~a~%" (get-universal-time) method uri )
      (with-open-file (log "~/access.log" :direction :output :if-exists :append
			   :if-does-not-exist :create)
	(prin1
	 (list (cons :time (get-universal-time))
	       (cons :method method)
	       (cons :peer (format nil "~a.~a.~a.~a" (nth 0 peer)
				   (nth 1 peer)
				   (nth 2 peer)
				   (nth 3 peer)))
	       (cons
		(cons :uri uri)
		(loop for key being the hash-key in headers using (hash-value val)
		   collect (cons key val))))
	 log)
	(terpri log))

      #+debug (format t "route='~a' exists=~a~%" route existsp)
      (if existsp
	  ;; call route
	  (funcall route req)
	  ;; else, 404
	  '(404
	    (("Host". "hackingrun.com")
	     ("Content-Length" . 11)
	     ("Content-Type" . "text/plain")
	     ("Connection" . "close")
	     )
	    "NOT FOUND
")))))

(defmacro defroute ((path method) &body body)
  (let ((name (gensym "HANDLER-")))
    `(progn
       (defun ,name (request)
	 ,@body)
       (setf (gethash ,path *routes*) #',name))))


;;
;;  (defroute "/" (req) ... )
;;  (defroute ("/" :GET) (req) ... )
;;  (defroute "/foo/bar/:userid:/" ...)

;; (defroute "/foo/bar/:uid:/")


