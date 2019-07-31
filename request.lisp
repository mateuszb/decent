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
  (headers (make-hash-table :test 'string= :synchronized t)))

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

(defun home (req)
  (let ((doc
	 (with-html-string
	   (:html
	    (:head (:title "::: hacking-run :::")
		   (:link :rel "stylesheet" :type "text/css" :href "/style.css"))
	    (:body (:div :id "logo" (:img :src "/logo.png"))
		   ;;(:h1 "HACKING RUN")
		   (loop for post in (read-posts)
		      collect
			(:div :class "post"
			      (:h3 (eval `(with-html ,(getf post :title))))
			      (:div :class "date" (eval `(with-html ,(getf post :date))))
			      (:p (eval `(with-html ,(getf post :text)))))))))))
    (list 200
	  (list
	   (cons "Host" "hackingrun.com")
	   (cons "Content-Length" (length doc))
	   (cons "Content-Type" "text/html")
	   ;;(cons "Connection" "close")
	   )
	  doc)))
(setf (gethash "/" *routes*) #'home)

(defun style-css (req)
  (let ((css "body { margin-top: 60px; margin-left: auto; margin-right: auto;
min-width: 100px; max-width: 50%; }
h1 { text-align: center; font-size: 32pt; } #logo { margin: auto; width: 128px; }
h3 { margin-bottom: 0px; font-size: 24pt;}
p { font-size: 14pt; }
img { max-width: 100%; max-height: 100%; }
.date { font-size: 12pt; margin-top: 0px; margin-bottom: 5px; color: gray;}
.post { margin-bottom: 50px; }
pre { font-size: 10pt; }"
	  ))
    (list 200
	  (list
	   (cons "Host" "hackingrun.com")
	   (cons "Content-Length" (length css))
	   (cons "Content-Type" "text/css"))
	  css)))
(setf (gethash "/style.css" *routes*) #'style-css)

(setf (gethash "/logo.png" *routes*)
      (lambda (req)
	(with-open-file (logo "~/blog/logo.png" :element-type '(unsigned-byte 8))
	  (let ((bytes (make-array (file-length logo) :element-type '(unsigned-byte 8))))
	    (read-sequence bytes logo)
	    (list
	     200
	     (list
	      '("Host" . "hackingrun.com")
	      '("Content-Type" . "image/png")
	      (cons "Content-Length" (length bytes)))
	     bytes)))))
