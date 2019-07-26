(in-package :decent)

(defparameter +BUFFER-SIZE+ 4096)


(defvar *requests*)
(defvar *listen-socket*)
(defvar *stop* nil)

(defun start (&optional (port 5000))
  (let ((dispatcher (make-dispatcher))
	(*connections* (make-hash-table)))
    (with-dispatcher (dispatcher)
      (let ((srv-socket (make-tcp-listen-socket port)))
	(set-non-blocking srv-socket)

	(on-read srv-socket #'accept-new-http-connection)

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

	(disconnect srv-socket)))))

(defun accept-new-http-connection (ctx event)
  (declare (ignore event))
  (with-slots ((listen-socket reactor.dispatch::socket)) ctx
    (loop
       do
	 (handler-case
	     (let ((new-socket (accept listen-socket)))
	       (set-non-blocking new-socket)
	       (setf (gethash new-socket *connections*) (make-http-connection new-socket))
	       (on-read new-socket #'http-rx-handler))
	   (operation-interrupted ()
	     (format t "accept() got interrupted. retrying...~%"))
	   (operation-would-block (err)
	     (format t "done accepting~%")
	     (loop-finish))))))

(defun http-tx-handler (ctx event)
  (with-slots ((socket reactor.dispatch::socket)) ctx
    (let ((conn (gethash socket *connections*)))
      (with-slots (txq) conn
	(let* ((elem (queue-peek txq))
	       (xfered (cdr (assoc :xfered elem)))
	       (size (cdr (assoc :size elem)))
	       (resp (cdr (assoc :data elem))))
	  (format t "xfered=~a~%" xfered)
	  (format t "size=~a~%" size)
	  (let ((nsent (send-response socket resp xfered (- size xfered))))
	    (format t "sent ~a of ~a bytes~%" nsent size)
	    (incf (cdr (assoc :xfered elem)) nsent)
	    (when (= (cdr (assoc :xfered elem)) size)
	      (dequeue txq)
	      ;(setf (gethash socket *connections*) conn)
	      (when (queue-empty-p txq)
		(del-write socket)))))))))

(defun split-receive (conn)
  (with-slots (socket rxbuf rxcap rd wr) conn
    (labels ((mask (x) (logand x (1- rxcap))))
      (let* ((size (- wr rd))
	     (nfree (- rxcap size))
	     (end (+ wr nfree)))
	(cond
	  ((< (mask end) (mask rd))
	   (let ((ptrs (list (sap+ (alien-sap rxbuf) (mask wr))
			     (alien-sap rxbuf)))
		 (lens (list (- rxcap (mask wr)) rd)))
	     (incf wr (receive socket ptrs lens))))

	  ((= (mask end) (mask rd))
	   (let ((ptr (sap+ (alien-sap rxbuf) (mask wr))))
	     (incf wr (receive socket (list ptr) (list nfree)))))

	  (t
	   (let ((ptr (sap+ (alien-sap rxbuf) (mask wr))))
	     (incf wr (receive socket (list ptr) (list nfree))))))))))

(defun try-receive (conn)
  (with-slots (rxbuf rxcap rd wr) conn
    (let* ((size (- wr rd))
	   (full? (= size rxcap)))
      (if full?
	  (error 'rx-buffer-full)
	  (let ((nread 0))
	    (handler-case (setf nread (split-receive conn))
	      (operation-would-block ()
		(format t "operation would block. will re-try later...~%"))))))))

(defun http-rx-handler (ctx event)
  (with-slots ((socket reactor.dispatch::socket)) ctx
    (handler-case
	(let ((conn (gethash socket *connections*)))
	  (try-receive conn)
	  (multiple-value-bind (complete? lines) (try-parse conn)
	    (format t "all lines=~a~%" lines)
	    (when complete?
	      (let* ((resp (process-request (parse-request lines)))
		     (txbuf (format-response resp)))
		(setf (slot-value conn 'lines) nil)
		(enqueue (list
			  (cons :xfered 0)
			  (cons :size (length txbuf))
			  (cons :data txbuf))
			 (slot-value conn 'txq))

		(on-write socket #'http-tx-handler)))))
      (socket-eof ()
	(let ((conn (gethash socket *connections*)))
	  (with-slots (socket) conn
	    (rem-socket socket)
	    (remhash socket *connections*)
	    (release-http-connection conn)))))))

(defun buf-char (buf pos)
  (code-char (deref buf pos)))

(defun ring-buffer-read-sequence (buf start end)
  (map 'string #'identity
       (loop for i from start below end
	  collect (buf-char buf (logand i 4095)))))

(defun try-parse (conn)
  (with-slots (socket rd wr rxcap rxbuf lines) conn
    (loop for i = rd then (1+ i) until (= i wr)
       with complete = nil
       when (and (char= (buf-char rxbuf (logand i (1- rxcap))) #\newline)
		 (char= (buf-char rxbuf (logand (1- i) (1- rxcap))) #\return))
       do
	 (let ((line (ring-buffer-read-sequence rxbuf rd (1- i))))
	   (incf rd (+ 2 (length line)))
	   (when (zerop (length line))
	     (setf complete t)
	     (loop-finish))
	   (push line lines))
       finally
	 (if lines
	     (setf rd (1+ i))
	     (setf rd i))
	 (return (values complete (if complete (nreverse lines) '()))))))

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
  (with-output-to-string (out)
    (princ (format-header resp) out)
    (princ (third resp) out)))

(defun send-response (socket resp pos len)
  (let ((buf (make-alien (signed 8) len)))
    (loop for i from 0 below len
       do
	 (setf (deref buf i) (char-code (aref resp (+ pos i)))))
    (let ((nsent (send socket (alien-sap buf) len)))
      (free-alien buf)
      nsent)))
