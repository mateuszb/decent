(in-package :decent)

(define-condition protocol-error ()
  ((protocol :initform nil :initarg :protocol)))

(defun is-separator? (c sep)
  (char= c sep))

(defun consume-token (in &optional (separator #\space))
  (loop for c = (peek-char nil in nil :eof)
     then (peek-char nil in nil :eof)
     until (or (eq :eof c) (is-separator? c separator))
     collect (read-char in) into tok
     finally
       (unless (eq :eof c)
	 (read-char in))
       (return (coerce tok 'string))))

(defun read-method (tok)
  (string-case (tok)
    ("GET" :GET)
    ("POST" :POST)
    ("HEAD" :HEAD)
    ("PUT" :PUT)
    ("DELETE" :DELETE)
    ("OPTIONS" :OPTIONS)
    ("TRACE" :TRACE)
    ("CONNECT" :CONNECT)
    (t :UNSUPPORTED-METHOD)))

(defun read-proto (tok)
  (string-case (tok)
    ("HTTP/1.1" :http1.1)
    ("HTTP/1.0" :http1.0)
    (t :unsupported-protocol)))

(defun find-separators (uri)
  (mapcan (lambda (x y)
	    (if (or (char= x #\?)) (list x y) nil))
	  (map 'list #'identity uri)
	  (loop for i from 0 below (length uri) collect i)))

(defun parse-uri-path (uri)
  (let ((path nil) (params nil))
    (let ((position (find-separators uri)))
      (if position
	  (setf path (subseq uri 0 (cadr position))
		params (subseq uri (1+ (cadr position))))
	  (setf path uri)))
    (cons path params)))

(defun parse-request-line (line)
  (with-input-from-string (in line)
    (let ((method (read-method (consume-token in)))
	  (uri (parse-uri-path (consume-token in)))
	  (proto (read-proto (string-upcase (consume-token in)))))
      (unless (member proto '(:http1.1 :http1.0))
	  (error (make-condition 'protocol-error :protocol proto)))
      (values method uri proto))))

(defun read-header-name (tok)
  (intern (string-upcase (subseq tok 0 (length tok))) :keyword))

(defun read-header-value (in)
  (let ((line (read-line in nil nil)))
    (when line
      (string-trim '(#\space) line))))

(defun parse-header-line (line)
  (with-input-from-string (in line)
    (let ((key (read-header-name (consume-token in #\:)))
	  (val (read-header-value in)))
      (values key val))))

(defun parse-request (peer lines)
  (let ((req (multiple-value-list (parse-request-line (car lines))))
	(hdrs
	 (loop for hdrline in (cdr lines)
	    with hdrs = (make-hash-table :test 'equal :synchronized t)
	    do
	      (multiple-value-bind (key val) (parse-header-line hdrline)
		#+debug
		(format t "~a: ~a~%" key val)
		(setf (gethash key hdrs) val))
	    finally (return hdrs))))
    (make-http-request
     :peer peer
     :method (first req)
     :uri (car (second req))
     :params (cdr (second req))
     :proto (third req)
     :headers hdrs)))
