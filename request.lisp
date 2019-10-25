(in-package :decent)

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
  (with-slots (peer method uri headers) req
    (funcall *handler* req)))

(defmacro serve (&rest forms)
  (let ((handle-fn (gensym))
	(reqvar (gensym))
	(certvar (gensym))
	(keyvar (gensym))
	(portvar (gensym))
	(port) (cert-path) (key-path)
	(passes))
    (loop for (hd . tail) on forms
       do
	 (case hd
	   (https
	    (assert (eq (car tail) 'on))
	    (assert (eq (cadr tail) 'port))
	    (setf port (caddr tail)))

	   (with
	    (case (car tail)
	      (certificate
	       (assert (eq (cadr tail) '=))
	       (setf cert-path (caddr tail)))
	      (key
	       (assert (eq (cadr tail) '=))
	       (setf key-path (caddr tail)))
	      (passes
	       (setf passes (cadr tail)))))))

    `(start :port ,port
	    :cert-path ,cert-path
	    :key-path ,key-path
	    :top-level-handler
	    (lambda (,reqvar)
	      ,(macroexpand
		`(expand-passes ,passes ,reqvar))))))

(defmacro expand-passes (passes reqvar)
  (reduce
   (lambda (&optional acc fn)
     (typecase fn
       (symbol `(funcall ,(symbol-function fn) ,acc))
       (list `(funcall ,(macroexpand fn) ,acc))))
   passes :initial-value reqvar))

(defmacro router (&rest forms)
  `(macrolet ((with-scope ((uri &rest args) &body routes)
		(format t "uri=~s, args=~s, routes=~s~%" uri args routes)
		`,routes)
	      (getm (uri handler)
		`(funcall ,handler req)))
     ,@forms))


;; we want to find a matching scope and then
;; use the scope's router


(defmacro with-scope (args &rest routes)
  (let ((reqvar (gensym))
	(slots (loop for (hd . tail) on args by #'cddr collect (intern (symbol-name hd) :keyword)))
	(vals  (loop for (hd . tail) on args by #'cddr collect (car tail))))

    `(lambda (,reqvar)
       (let ((*router* (make-router)))
	 ,@(loop for r in routes
	      collect
		(destructuring-bind (method regex handler) r
		  `(add-route *router* (make-route ,method ,regex ,(symbol-function handler)))))
	 (with-slots (headers) ,reqvar
	   (when (and ,@(loop
			   for k in slots
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
				`(= (parse-integer (gethash ,k headers "0")) ,v)))))
	     (dispatch (routes *router*) ,reqvar)))))))

(defmacro router-pass (&body scopes)
  (let ((resultvar (gensym))
	(reqvar (gensym))
	(itervar (gensym)))
    `(lambda (,reqvar)
       (format t "request var=~a~%" ,reqvar)
       (loop for k being the hash-key in (http-request-headers ,reqvar)
	  using (hash-value val)
	  do (format t "~s=~s~%" k val))
       (loop for ,itervar in (list ,@scopes)
	  with ,resultvar = nil
	  while (null ,resultvar)
	  do
	    (setf ,resultvar (funcall ,itervar ,reqvar))
	    (when ,resultvar
	      (loop-finish))
	  finally (return ,resultvar)))))

'(serve https on port 8443
  with certificate = "path/to/cert.der"
  with key = "path/to/key.der"
  with passes
  ((router
     (with-scope (host "example.com")
       (:get "/" front-page)
       (:get "/login" login-page)
       (:post "/login" login-page-submitted)))))
