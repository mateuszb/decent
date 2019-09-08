(in-package :decent)

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

(defun log-request (req)
  (with-slots (peer method uri headers) req
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
      (terpri log))))

(defun process-request (req)
  (log-request req)
  (with-slots (peer method uri headers) req
    (dispatch (routes *router*) method uri)))
