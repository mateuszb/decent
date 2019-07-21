(in-package :decent)

(defparameter +BUFFER-SIZE+ 4096)

(defstruct ringbuffer
  (buffer (make-array +BUFFER-SIZE+ :element-type '(unsigned-byte 8)))
  (capacity +BUFFER-SIZE+)
  (read 0)
  (write 0))

(defstruct http-connection
  (rxbuf (make-ringbuffer))
  (rxpos 0)

  (complete nil)
  (lines nil)
  (txq (make-queue 16)))

(defun uwrap (x)
  (logand x #xFFFFFFFF))

(defun mask (x)
  (logand x (1- +BUFFER-SIZE+)))

(defun ring-buffer-size (buf)
  (with-slots (read write) buf
    (- write read)))

(defun ring-buffer-full? (buf)
  (with-slots (buffer) buf
    (= (ring-buffer-size buf) (car (array-dimensions buffer)))))

(defun ring-buffer-empty? (buf)
  (with-slots (read write) buf
    (= read write)))

(defun ring-buffer-free (buf)
  (with-slots (buffer) buf
    (- (car (array-dimensions buffer))
       (ring-buffer-size buf))))

(defun advance (buf n)
  (with-slots (write buffer) buf
    (incf write n)))

(defun consume (buf n)
  (with-slots (read buffer) buf
    (incf read n)))

(defvar *connections*)
(defvar *requests*)

(defun make-listen-socket (port)
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (setf (sockopt-reuse-address socket) t)
    (socket-bind socket #(0 0 0 0) port)
    (setf (non-blocking-mode socket) t)
    socket))

(defun start-listening (socket)
  (socket-listen socket 10))

(defun stop-listening (socket)
  (socket-close socket))

(defvar *stop* nil)
(defun start (&optional (port 5000))
  (sb-thread:make-thread
   (lambda ()
     (let ((dispatcher (make-dispatcher))
	   (*connections* (make-hash-table)))
       (with-dispatcher (dispatcher)
	 (let ((accept-socket (make-listen-socket port)))
	   (on-read accept-socket #'accept-new-http-connection)

	   (start-listening accept-socket)
	   (loop until *stop*
	      do
		(format t "waiting for events...~%")
		(dispatch-events (wait-for-events)))

	   (loop for sock being the hash-key in *connections*
	      using (hash-value val)
	      do
		(rem-socket sock)
		(remhash sock *connections*)
		(socket-close sock))

	   (stop-listening accept-socket)))))))

(defun accept-new-http-connection (ctx event)
  (declare (ignore event))
  (format t "new connection handler~%")
  (with-slots ((listen-socket reactor.dispatch::socket)) ctx
    (let ((new-socket nil))
      (setf new-socket (socket-accept listen-socket))
      (setf (non-blocking-mode new-socket) t)
      (setf (gethash new-socket *connections*) (make-http-connection))
      (format t "about to add rx handler...~%")
      (on-read new-socket #'http-rx-handler)
      t)))

(defun make-displaced-buffer (buf pos len &optional (type '(unsigned-byte 8)))
  (make-array len
	      :element-type type
	      :displaced-to buf
	      :displaced-index-offset pos))

(defun displaced-receive (socket buf pos len)
  (let ((buf (make-displaced-buffer buf pos len)))
    (multiple-value-bind (buf len) (socket-receive socket buf len)
      len)))

(defun split-receive (socket http-conn bytes-to-read)
  (let ((rxring (slot-value http-conn 'rxbuf)))
    (with-slots (buffer read write capacity) rxring
      (unless (ring-buffer-full? rxring)
	(let* ((free-space (ring-buffer-free rxring))
	       (read-size (min free-space bytes-to-read)))
	  (if (<= (mask (+ write read-size)) (mask write))
	      ;; case 1:
	      ;; [.....xxxxxx........]
	      ;;       ^     ^
	      ;;     read   write
	      (let ((readlen1 (displaced-receive socket buffer (mask write) (- capacity (mask write)))))
		(advance rxring readlen1)
		(if (> (- bytes-to-read readlen1) 0)
		    (let ((readlen2 (displaced-receive socket buffer 0 (mask read))))
		      (advance rxring readlen2)
		      (+ readlen1 readlen2))
		    readlen1))
	      ;; case 1:
	      ;; [xxxxx........xxxxxx]
	      ;;       ^       ^
	      ;;     write    read
	      (let ((readlen (displaced-receive socket buffer (mask write) read-size)))
		(advance rxring readlen)
		readlen)))))))

(defun http-tx-handler (ctx event)
  (with-slots ((socket reactor.dispatch::socket)) ctx
    (format t "TX handler called with event '~a'~%" event)
    (let ((conn (gethash socket *connections*)))
      (with-slots (txq) conn
	(let* ((elem (queue-peek txq))
	       (xfered (cdr (assoc :xfered elem)))
	       (size (cdr (assoc :size elem)))
	       (resp (cdr (assoc :data elem)))
	       (dispbuf (make-displaced-buffer resp xfered (- size xfered) 'character)))
	  (format t "xfered=~a~%" xfered)
	  (format t "size=~a~%" size)
	  (let ((nsent (send-response socket dispbuf)))
	    (format t "sent ~a of ~a bytes~%" nsent size)
	    (incf (cdr (assoc :xfered elem)) nsent)
	    (when (= (cdr (assoc :xfered elem)) size)
	      (dequeue txq)
	      (setf (gethash socket *connections*) conn)
	      (when (queue-empty-p txq)
		(format t "tx queue is empty. disabling :out filter notification on this socket~%")
		;;(del-write socket)
		(rem-socket socket)
		(remhash socket *connections*)
		(socket-close socket)))))))))

(defun http-rx-handler (ctx event)
  (with-slots ((socket reactor.dispatch::socket)) ctx
    (format t "RX handler called with event '~a'~%" event)
    (let ((rxbytes (min 4096 (getf event :bytes-in)))
	  (conn (gethash socket *connections*)))
      (with-slots (rxbuf lines complete) conn
	(unless (ring-buffer-full? rxbuf)
	  (when (> rxbytes 0)
	      (progn
		(split-receive socket conn rxbytes)
		(if (try-parse conn)
		    (let* ((reqlines (nreverse lines))
			   (resp (process-request (parse-request reqlines)))
			   (txbuf (format-response resp)))

		      ;; append response to the send queue
		      (enqueue (list
				(cons :xfered 0)
				(cons :size (length txbuf))
				(cons :data txbuf))
			       (slot-value conn 'txq))

		      ;; enable on-write events
		      (on-write socket #'http-tx-handler)
		      (setf lines '() complete nil))))))))))

(defun buf-char (buf pos)
  (code-char (aref buf pos)))

(defun ring-buffer-read-sequence (buf start end)
  (map 'string #'code-char
       (loop for i from start below end
	  collect (aref buf (mask i)))))

(defun try-parse (req)
  (with-slots (rxbuf rxpos lines complete) req
    (with-slots (buffer read write capacity) rxbuf
      (loop for i = rxpos then (1+ i)
	 until (= i write)
	 when (and (char= (buf-char buffer (mask i)) #\newline)
		   (char= (buf-char buffer (mask (1- i))) #\return))
	 do
	   (let ((line (ring-buffer-read-sequence buffer read (1- i))))
	     (consume rxbuf (+ 2 (length line)))
	     (setf complete (zerop (length line)))
	     (if complete
		 (loop-finish)
		 (push line lines)))
	 finally
	   (setf rxpos (1+ i))
	   (return complete)))))

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
      (format out (status-line "HTTP/1.1" code "OK"))
      (loop for (hdr . val) in hdrs
	 do
	   (format out "~a: ~a~a~a" hdr val #\return #\newline))
      (format out "~a~a" #\return #\newline))))

(defun format-response (resp)
  (with-output-to-string (out)
    (princ (format-header resp) out)
    (princ (third resp) out)))

(defun send-response (socket resp)
  (socket-send socket resp (length resp)))
