(in-package :decent)

(defparameter +BUFFER-SIZE+ 4096)

(defvar *requests*)
(defvar *listen-socket*)
(defvar *stop* nil)
(defvar *ssl-context*)
(defvar *hostname* "localhost")
(defvar *request* nil
  "The current http request being processed")
(defparameter +MAX-REQUEST-BODY-SIZE+ (* 8192 10))

(defun start (&optional &key (https-p t) (port 5000) cert-path key-path)
  (let ((dispatcher (make-dispatcher))
	(*connections* (make-hash-table))
	(*ssl-context* (if https-p (tls:make-tls-context nil "/etc/ssl/certs" t) nil)))
    (with-dispatcher (dispatcher)
      (tls:with-tls-context *ssl-context*
	(let ((srv-socket (make-tcp-listen-socket port)))
	  (set-non-blocking srv-socket)

	  (if https-p
	      (progn
		(tls:load-cert cert-path)
		(tls:load-key key-path)
		(on-read srv-socket #'accept-https-connection))
	      (on-read srv-socket #'accept-http-connection))

	  (loop until *stop*
	     do
	       (let ((events (wait-for-events)))
		 (dispatch-events events)))

	  (loop for sock being the hash-key in *connections*
	     using (hash-value val)
	     do
	       (rem-socket sock)
	       (remhash sock *connections*)
	       (disconnect sock))

	  (disconnect srv-socket))))))

(defun accept-http-connection (ctx event)
  (declare (ignore event))
  (with-slots ((listen-socket reactor.dispatch::socket)) ctx
    (loop
       do
	 (handler-case
	     (let ((new-socket (accept listen-socket)))
	       (set-non-blocking new-socket)
	       (setf (gethash new-socket *connections*) (make-http-connection new-socket))
	       (on-read new-socket #'http-rx-handler)
	       (on-disconnect new-socket #'http-disconnect-handler))
	   (operation-interrupted ()
	     (format t "accept() got interrupted. retrying...~%"))
	   (operation-would-block ()
	     (loop-finish))))))

(defun accept-https-connection (ctx event)
  (declare (ignore event))
  (with-slots ((listen-socket reactor.dispatch::socket)) ctx
    (loop
       do
	 (handler-case
	     (let ((new-socket (accept listen-socket)))
	       (set-non-blocking new-socket)
	       (setf (gethash new-socket *connections*) (make-https-connection new-socket))
	       (on-read new-socket #'https-connect-handler)
	       (on-disconnect new-socket #'http-disconnect-handler))
	   (operation-interrupted ()
	     (format t "accept() got interrupted. retrying...~%"))
	   (operation-would-block ()
	     (loop-finish))))))

(defun https-connect-handler (ctx event)
  "Performs the TLS handshake and establishes the HTTPS connection.
This function is called on write ready event from a reactor."
  (declare (ignorable event))
  (let* ((socket (context-socket ctx))
	 (httpsconn (gethash socket *connections*))
	 (ctxdata (context-data ctx))
	 (tls (if ctxdata
		  (https-tls-context ctxdata)
		  (tls:accept (socket:socket-fd socket)))))
    (unless ctxdata
      (format t "context is ~a~%" ctxdata)
      (with-slots (ssl-context ssl-stream) httpsconn
	(format t "setting streams and context~%")
	(setf ssl-context tls
	      ssl-stream (tls:make-tls-stream *ssl-context* tls)))
      (setf (context-data ctx) httpsconn))

    (format t "https connect handler about to perform a handshake evt=~a~%" event)
    (let ((err (tls:ssl-do-handshake tls)))
      (cond
	((= err 1)
	 (format t "handshake complete~%")
	 (on-read socket #'http-rx-handler)) ; switch to normal processing
	((<= err 0)
	 (format t "handshake returned ... ~A~%" err)
	 (let ((code (tls::ssl-get-error tls err)))
	   (cond
	     ((= code tls:+SSL-ERROR-WANT-READ+)
	      (format t "want read~%")) ; do nothing?
	     ((= code tls:+SSL-ERROR-WANT-WRITE+)
	      (format t "want write~%")
	      (del-read socket) ; switch to write polling
	      (on-write socket #'https-connect-handler))
	     (t (http-disconnect-handler ctx event)))))))))

(defun http-tx-handler (ctx event)
  (declare (ignore event))
  (with-slots ((socket reactor.dispatch::socket)) ctx
    (handler-case
	(let ((conn (gethash socket *connections*)))
	  (with-slots (txq) conn
	    (let* ((elem (queue-peek txq))
		   (xfered (cdr (assoc :xfered elem)))
		   (size (cdr (assoc :size elem)))
		   (resp (cdr (assoc :data elem)))
		   (req (cdr (assoc :request elem)))
		   (hdrs (http-request-headers req))
		   (nsent 0))
	      (tagbody
	       continue
		 (setf nsent
		       (send-response (if (is-https-p conn) conn socket)
				      resp
				      xfered
				      (- size xfered)))
		 (format t "before send xfered: ~a~%" (assoc :xfered elem))
		 (incf (cdr (assoc :xfered elem)) nsent)
		 (incf xfered nsent)
		 (format t "after send xfered: ~a~%" (assoc :xfered elem))
		 (cond
		   ((= (cdr (assoc :xfered elem)) size)
		    (format t "dequeuing response after completion.~%")
		    (dequeue txq)
		    (when (queue-empty-p txq)
		      (del-write socket))
		    (when (string= (string-downcase (gethash :connection hdrs)) "close")
		      (rem-socket socket)
		      (remhash socket *connections*)
		      (release-connection conn)))
		   ((< (cdr (assoc :xfered elem)) size)
		    (go continue)))))))
      (tls:tls-wants-read ()
	(on-read socket #'http-tx-handler))

      (tls:tls-wants-write ()
	t)

      (tls:tls-zero-return ()
	(rem-socket socket)
	(release-connection (gethash socket *connections*))
	(remhash socket *connections*))

      (operation-would-block () t)
      
      )))

(defun http-disconnect-handler (ctx event)
  (with-slots ((socket reactor.dispatch::socket)) ctx
    (let ((conn (gethash socket *connections*)))
      (when conn
	(with-slots (socket) conn
	  (rem-socket socket)
	  (remhash socket *connections*)
	  (release-connection conn))))))

(defun wraps-around? (start len cap)
  (>= (+ start len) cap))

(defun split-receive (conn)
  (with-slots (socket rxbuf rxcap rd wr) conn
    (labels ((mask (x) (logand x (1- rxcap))))
      (let* ((size (- wr rd))
	     (nfree (- rxcap size))
	     (end (+ wr nfree)))
	(if (wraps-around? (mask wr) nfree rxcap)
	    (let ((ptrs (list (sap+ (alien-sap rxbuf) (mask wr))
			      (alien-sap rxbuf)))
		  (lens (list (- rxcap (mask wr)) (mask rd))))
	      #+debug
	      (format t "#1a. receive into pos ~a of ~a bytes~%" (mask wr) (- rxcap (mask wr)))
	      #+debug
	      (format t "#1b. receive into pos ~a of ~a bytes~%" 0 (mask rd))
	      (incf wr (receive socket ptrs lens)))
	    (let ((ptr (sap+ (alien-sap rxbuf) (mask wr))))
	      #+debug
	      (format t "#3. single receive into pos ~a of ~a bytes~%" (mask wr) nfree)
	      (incf wr (receive socket (list ptr) (list nfree)))))))))

(defgeneric try-receive (connection))

(defmethod try-receive ((conn http-connection))
  (with-slots (rxbuf rxcap rd wr) conn
    (let* ((size (- wr rd))
	   (full? (= size rxcap)))
      (if full?
	  (error 'rx-buffer-full)
	  (let ((rxbytes (split-receive conn)))
	    #+debug
	    (format t "read ~a bytes~%" rxbytes)
	    rxbytes)))))

(defmethod try-receive ((conn https-connection))
  (tls:tls-read (https-tls-stream conn)))

(define-condition waiting-for-body (condition) ())

(defun https-rx-body (ctx event)
  (declare (ignorable event))
  (format t "rx body~%")
  (handler-case
      (let* ((socket (context-socket ctx))
	     (conn (gethash socket *connections*))
	     (request (slot-value conn 'request))
	     (hdrs (http-request-headers request))
	     (content-len (min +MAX-REQUEST-BODY-SIZE+
			       (parse-integer (gethash :content-length hdrs))))
	     (tls (https-tls-stream conn))
	     (bytes-avail (tls::tls-stream-read-bytes-available tls)))
	(when (> content-len 0)
	  (if (>= bytes-avail content-len)
	      ;; we can already read everything...
	      (with-slots (body body-sofar) request
		(setf body (tls:tls-read-byte-sequence tls content-len)
		      body-sofar content-len)
		(let* ((resp (process-request request))
		       (txbuf (format-response resp)))
		  (setf (slot-value conn 'lines) nil)
		  (enqueue (list
			    (cons :xfered 0)
			    (cons :size (length txbuf))
			    (cons :data txbuf)
			    (cons :request request))
			   (slot-value conn 'txq))
		  (on-write socket #'http-tx-handler))		
		(on-read socket #'http-rx-handler))
	      ;; not enough data available. we will probably split the
	      ;; read across many invocations of the http-rx-body
	      (progn
		(with-slots (body body-sofar) request
		 (format t "not enough data available in the buffer.~%")
		 (unless body
		   (format t "initializing body slot~%")
		   (setf body (make-array content-len :element-type '(unsigned-byte 8))
			 body-sofar 0)))
	      (tagbody
	       read-again
		 (with-slots (body body-sofar) request
		   (format t "read-again tag with bodysofar=~a~%" body-sofar)
		   (let ((seq (tls:tls-read-byte-sequence tls (- content-len body-sofar))))
		     (format t "read partial sequence of len ~a~%" (length seq))
		     (when seq
		       (loop for i from 0 below (length seq)
			  for elem across seq
			  do
			    (setf (aref body body-sofar) elem)
			    (incf body-sofar))))
		   (format t "body-sofar=~a~%" body-sofar)
		   (cond
		     ((= content-len body-sofar)
		      ;; full body arrived. process the request
		      (format t "~a~%" (slot-value request 'body))
		      (let* ((resp (process-request request))
			     (txbuf (format-response resp)))
			(setf (slot-value conn 'lines) nil)
			(enqueue (list
				  (cons :xfered 0)
				  (cons :size (length txbuf))
				  (cons :data txbuf)
				  (cons :request request))
				 (slot-value conn 'txq))
			(on-write socket #'http-tx-handler)))
		     ((< body-sofar content-len)
		      ;; still need to read more of the body
		      (format t "we are missing ~a bytes of the body~%"
			      (- content-len body-sofar))
		      (tls:tls-read tls)
		      (go read-again)))))))))
    (tls:tls-wants-read ()
      ;; do nothing.. we will be called back again by the reactor when
      ;; data arrives
      (format t "read ran out of data on read.~%")
      (on-read (context-socket ctx) #'https-rx-body)
      (return-from https-rx-body))
    (parse-error ()
      (rem-socket (context-socket ctx))
      (release-connection (gethash (context-socket ctx) *connections*))
      (remhash (context-socket ctx) *connections*))))

(defun http-rx-handler (ctx event)
  (with-slots ((socket reactor.dispatch::socket)) ctx
    (handler-case
	(let* ((conn (gethash socket *connections*))
	       (peer (get-peer-name (socket-fd socket))))
	  (tagbody
	   read-again
	     (multiple-value-bind (complete? lines) (try-parse conn)
	       (format t "try-parse returned ~a ~a~%" complete? lines)
	       (when (and lines (not complete?))
		 (error "request parsing problem?~%"))
	       (when (and complete? lines)
		 (let ((req (parse-request peer lines)))
		   (setf (slot-value conn 'request) req)
		   (if (gethash :content-length (http-request-headers req))
		       (https-rx-body ctx event)
		       (let* ((resp (process-request req))
			      (txbuf (format-response resp)))
			 (setf (slot-value conn 'lines) nil)
			 (enqueue (list
				   (cons :xfered 0)
				   (cons :size (length txbuf))
				   (cons :data txbuf)
				   (cons :request req))
				  (slot-value conn 'txq))
			 (on-write socket #'http-tx-handler))))))
	     (try-receive conn)
	     (go read-again)))
      (tls:tls-wants-read ()  (format t "tls operation would block on read.~%"))

      (tls:tls-wants-write () (format t "tls operation would block on write.~%"))

      (tls:tls-zero-return ()
	(rem-socket socket)
	(release-connection (gethash socket *connections*))  
	(remhash socket *connections*))

      (operation-would-block ()
	#+debug (format t "operation would block. will re-try later...~%")
	t)
      (protocol-error (err)
	;; someone sent us a wrong protocol version we don't support.
	;; close the socket. don't bother sending any responses out.
	(let ((conn (gethash socket *connections*)))
	  (rem-socket socket)
	  (remhash socket *connections*)
	  (release-connection conn)))
      (socket-read-error ()
	(let ((conn (gethash socket *connections*)))
	  (with-slots (socket) conn
	    (rem-socket socket)
	    (remhash socket *connections*)
	    (release-connection conn))))
      (socket-eof ()
	(let ((conn (gethash socket *connections*)))
	  (with-slots (socket) conn
	    (rem-socket socket)
	    (remhash socket *connections*)
	    (release-connection conn)))))))

(defun buf-char (buf pos)
  (code-char (deposit-field (deref buf pos) (byte 8 0) 0)))

(defun ring-buffer-read-sequence (buf start end)
  ;(format t "reading buffer from ~a to ~a~%" start end)
  (map 'string #'identity
       (loop for i from start below end
	  collect (buf-char buf (logand i 4095)))))

(defgeneric try-parse (conn))

(defmethod try-parse ((conn http-connection))
  (with-slots (socket rd rdsofar wr rxcap rxbuf lines) conn
    (loop for i = rdsofar then (1+ i) until (= i wr)
       with complete = nil
       do
	 (setf rdsofar i)
       when (and (char= (buf-char rxbuf (logand i (1- rxcap))) #\newline)
		 (char= (buf-char rxbuf (logand (1- i) (1- rxcap))) #\return))
       do
	 (let ((line (ring-buffer-read-sequence rxbuf rd (1- i))))
	   (setf rd (1+ i)
		 rdsofar (1+ i))
	   (when (zerop (length line))
	     (setf complete t)
	     (loop-finish))
	   (push line lines))
       finally
	 (return (values complete (if complete (nreverse lines) '()))))))

(defmethod try-parse ((conn https-connection))
  (with-slots (socket lines ssl-context ssl-stream) conn
    (loop
       for line = (tls::tls-read-line ssl-stream :CRLF)
       then (tls::tls-read-line ssl-stream :CRLF)
       with complete = nil
       while line
       do
	 (format t "read line: '~a'~%" line)
       if (and line (zerop (length line))) do
	 (setf complete t)
	 (loop-finish)
       else do
	 (push line lines)
       finally
	 (format t "terminating the loop: complete? = ~a~%" complete)
	 (return (values complete
			 (if complete
			     (nreverse lines)
			     nil))))))

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

  (format t "sending response ~a-~a (~a bytes)~%" pos (+ pos len) len)  
  (let ((nwritten (tls::tls-write-byte-sequence
		   (https-tls-stream conn)
		   (subseq resp pos (+ pos len)))))
    (format t "nwritten=~a~%" nwritten)
    nwritten))
