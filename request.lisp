(in-package :decent)

(defvar *routes* (make-hash-table :test 'equal))

(defclass route (t)
  ((path :type 'string)
   (method :type 'keyword)))

(defstruct http-request
  (method nil)
  (uri nil)
  (params nil)
  (proto nil)
  (headers (make-hash-table :test 'string= :synchronized t)))

(defun process-request (req)
  (with-slots (method uri) req
    (multiple-value-bind (route existsp) (gethash uri *routes*)
      (format t "route='~a' exists=~a~%" route existsp)
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

;; example routes
(defun read-posts ()
  (with-open-file (in "~/blog/posts.lisp")
    (read in)))

(setf (gethash "/" *routes*)
      (lambda (req)
	(let ((doc (with-html-string
		     (:html
		      (:head (:title "::: hacking-run :::"))
		      (:body (:h1 "Hacking Run")
			     (loop for post in (read-posts)
				collect (:div
					 (:h3 (getf (cdr post) :title))
					 (:h4 (getf (cdr post) :date))
					 (:p (getf (cdr post) :text)))))))))
	  (list 200
		(list
		 (cons "Host" "hackingrun.com")
		 (cons "Content-Length" (length doc))
		 (cons "Content-Type" "text/html")
		 ;(cons "Connection" "close")
		 )
		doc))))
