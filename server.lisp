(in-package :decent)

(declaim (optimize (debug 3) (speed 0)))

(defparameter +BUFFER-SIZE+ 8192)

(defvar *requests*)
(defvar *listen-socket*)
(defvar *stop* nil)
(defvar *hostname* "localhost")
(defvar *request* nil
  "The current http request being processed")

(defvar *handler*)

(defparameter +MAX-REQUEST-BODY-SIZE+ (* 8192 10))

(defun start (&key (port 5000) cert-path key-path top-level-handler)
  (let ((*handler* top-level-handler))
    (start-server
     cert-path
     key-path
     port
     #'http-accept-connection
     #'http-rx-handler
     nil;#'http-tx-handler
     nil
     #'http-disconnect-handler)))

(defun http-accept-connection (tlsctx)
  (format t "TLS connection accepted~%")
  (make-https-connection tlsctx))

(defun http-disconnect-handler (ctx event)
  (declare (ignore event))
  (with-slots (handle) ctx
    (let ((conn (gethash (handle-key handle) *connections*)))
      (when conn
	(rem-handle handle)
	(remhash (handle-key handle) *connections*)
	(release-connection conn)))))

(define-condition waiting-for-body (condition) ())

(defun bad-request (http-conn)
  (let ((tls (tls http-conn)))
    (tls-write
     tls (concatenate
	  'string (format nil "HTTP/1.1 400 Bad request")
	  '(#\return #\newline) '(#\return #\newline)))
    (tls-close tls)))

(defun not-found (http-conn)
  '(404
    (("Host". "hackingrun.com")
     ("Content-Length" . 11)
     ("Content-Type" . "text/plain")
     ("Connection" . "close")
     )
    "NOT FOUND
"))

(defun http-rx-handler (http-conn bytes-to-read)
  (format t "bytes to read: ~a~%" bytes-to-read)
  ;; try parsing the rxbuffer and see if we can get a complete request
  ;; out of it
  (handler-case
      (progn
	(unless (http-request http-conn)
	  (let* ((lines (try-parse http-conn))
		 (tls (tls http-conn))
		 (peer (get-peer-name (socket-fd (tls:socket tls)))))
	    (when lines
	      (format t "lines=~a~%" lines)
	      (let ((req (parse-request peer lines)))
		(setf (http-request http-conn) req)))))

	(let* ((tls (tls http-conn))
	       ;(peer (get-peer-name (socket-fd (tls:socket tls))))
	       (req (http-request http-conn))
	       (hdrs (http-request-headers req))
	       (val (gethash :content-length hdrs "0"))
	       (conn-hdr (gethash :connection hdrs "close"))
	       (len (when val (parse-integer val))))

	  (when (> len (stream-size (tls::rx-data-stream tls)))
	    (return-from http-rx-handler))

	  (when (> len 0)
	    (let ((array (make-array len :element-type '(unsigned-byte 8))))
	      (read-sequence array (tls::rx-data-stream tls))
	      (setf (http-request-body req) array)))

	  (let* ((response (process-request req)))
	    (when (and conn-hdr response)
	      (setf (cadr response)
		    (cons (cons "Connection" conn-hdr) (cadr response))))

	    (cond
	      ((null response)
	       (tls-write
		(tls http-conn)
		(format-response (not-found http-conn))))
	      (t
	       (let ((bytes (format-response response)))
		 (tls-write (tls http-conn) bytes))))

	    (setf (http-request http-conn) nil))))

    (protocol-error (e)
      (format t "Client requested unsupported protocol: ~a~%" (protocol e))
      (bad-request http-conn))))

(defun buf-char (buf pos)
  (code-char
   (deposit-field
    (deref buf pos) (byte 8 0) 0)))

(defun parse-single-line (stream pos)
  (let ((size (stream-size stream)))
    (loop
       for i = pos then (1+ i)
       for curr = (stream-peek-char stream i) then (stream-peek-char stream i)
       for prev = (stream-peek-char stream (1- i)) then (stream-peek-char stream (1- i))
       with line = nil
       while (and (< i size) curr)
       when (and (char= curr #\newline) (and prev (char= prev #\return)))
       do
	 (let ((linesize (1- i)))
	   (setf line (make-array linesize :element-type '(unsigned-byte 8)))
	   (read-sequence line stream)
	   (read-byte stream)  ; read and discard the extra #\return byte
	   (read-byte stream)) ; read and discard the extra #\newline byte
       until (and (char= curr #\newline) (and prev (char= prev #\return)))
       finally
	 (return line))))

(defun try-parse (http-conn)
  (let ((stream (tls::rx-data-stream (tls http-conn))))
    (tagbody
     next-line
       (let ((line (parse-single-line stream (last-parse-pos http-conn))))
	 (cond
	   ((null line) ; no complete line returned yet. stop parsing.
	    (return-from try-parse nil))

	   ((= (length line) 0)
	    (let ((result (nreverse (request-lines http-conn))))
	      (setf (request-lines http-conn) nil)
	      (return-from try-parse result)))

	   (t ; normal line
	    (push (map 'string #'code-char line)
		  (request-lines http-conn))
	    (setf (last-parse-pos http-conn) 0)
	    (go next-line)))))))

(defun status-line (proto code reason)
  (with-output-to-string (out)
    (format out "~a ~a ~a~a~a" proto code reason #\return #\newline)))

(defun hash->acons (hdrs)
  (loop for key being the hash-key in hdrs
     using (hash-value val)
     collect (cons key val)))

(defun format-header (resp)
  (destructuring-bind (code hdrs body) resp
    (with-output-to-string (out)
      (format out (status-line "HTTP/1.1" code (if (= code 200) "OK" "NOT FOUND")))
      (loop for (hdr . val) in hdrs
	 do
	   (format out "~a: ~a~a~a" hdr val #\return #\newline))
      (format out "~a~a" #\return #\newline))))

(defun format-response (resp)
  (let* ((hdr (format-header resp))
	 (hdrlen (length hdr))
	 (msg (third resp))
	 (msglen (length msg))
	 (len (+ msglen hdrlen))
	 (bytes (make-array len :element-type '(unsigned-byte 8))))
    (loop for c across hdr
       for i from 0 below (length hdr)
       do (setf (aref bytes i) (char-code (aref hdr i))))
    (loop for b across msg
       for i from 0 below msglen
       for j from hdrlen below len
       do
	 (assert (< j len))
	 (setf (aref bytes j)
		(typecase (aref msg i)
		  (character (char-code (aref msg i)))
		  (t (aref msg i)))))
    bytes))

(defgeneric send-response (conn-or-socket resp pos len)
  (:documentation "Send the response bytes over the corresponding connection stream"))

(defmethod send-response ((socket integer) resp pos len)
  (when (zerop len)
    (error "zero length response size?"))
  (let ((buf (make-alien (unsigned 8) len)))
    ;(format t "allocated ~a bytes of alien buf ~a~%" len (alien-sap buf))
    (loop for i from 0 below len
       do
	 (setf (deref buf i) (aref resp (+ pos i))))
    (let ((nsent (send socket (alien-sap buf) len)))
      ;(format t "freeing ~a bytes of alien buf ~a~%" len (alien-sap buf))
      (free-alien buf)
      nsent)))

(defmethod send-response ((conn https-connection) resp pos len)
  (when (zerop len)
    (error "zero length response size?"))

  (let ((nwritten (tls::tls-write-byte-sequence
		   (https-tls-stream conn)
		   (subseq resp pos (+ pos len)))))
    nwritten))
