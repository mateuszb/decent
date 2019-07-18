(in-package :decent)

(defstruct http-connection
  (rxbuf (make-array 4096 :element-type '(unsigned-byte 8)))
  (rxstart 0)
  (rxpos 0)
  (rxlen 0)
  (complete nil)
  (lines nil)
  (txbuf (make-array 4096 :element-type '(unsigned-byte 8)))
  (txpos 0)
  (txlen 0))

(defvar *connections*)

(defun make-listen-socket (port)
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (setf (sockopt-reuse-address socket) t)
    (socket-bind socket #(0 0 0 0) port)
    (setf (non-blocking-mode socket) t)
    socket))

(defun start-listening (socket)
  (socket-listen socket 10))

(defvar *stop* nil)
(defun start (&optional (port 5000))
  (let ((dispatcher (make-dispatcher))
	(*connections* (make-hash-table)))
    (with-dispatcher (dispatcher)
      (let ((accept-socket (make-listen-socket port)))
	(on-read accept-socket #'accept-new-http-connection)

	(start-listening accept-socket)
	(loop until *stop*
	   do
	     (dispatch-events (wait-for-events)))))))

(defun accept-new-http-connection (ctx event)
  (format t "new http connection acceptor~%")
  (with-slots ((listen-socket reactor.dispatch::socket)) ctx
    (let ((new-socket nil))
      (setf new-socket (socket-accept listen-socket))
      (setf (non-blocking-mode new-socket) t)
      (setf (gethash new-socket *connections*) (make-http-connection))
      (on-read new-socket #'http-rx-handler)
      t)))

(defun make-displaced-buffer (buf pos)
  (make-array (- (car (array-dimensions buf)) pos)
	      :element-type '(unsigned-byte 8)
	      :displaced-to buf
	      :displaced-index-offset pos))

(defun http-rx-handler (ctx event)
  (format t "HTTP rx handler~%")
  (with-slots ((socket reactor.dispatch::socket)) ctx
    (let ((rxbytes (getf event :bytes-in))
	  (conn (gethash socket *connections*)))
      (with-slots (rxbuf rxlen) conn
	(multiple-value-bind (buf len peer)
	    (let ((buffer-view (make-displaced-buffer rxbuf rxlen)))
	      (socket-receive socket buffer-view (min (- (car (array-dimensions rxbuf)) rxlen) rxbytes)))
	  (incf rxlen len))

	(if (try-parse conn)
	    (format t "request complete. lines=~s~%" (nreverse (slot-value conn 'lines)))
	    (format t "request incomplete.~%"))
	;; TODO: check if we received everything in full
	)
      )
    (format t "event=~a~%" event)
    (format t "socket=~a~%" socket)))

(defun try-parse (req)
  (with-slots (rxstart rxpos rxlen rxbuf lines complete) req
    (loop for i from rxpos below rxlen
       if
	 (and
	  (= (aref rxbuf i) (char-code #\Newline))
	  (and (> i 0) (= (aref rxbuf (1- i)) (char-code #\return))))
       do
	 (if (= (- i rxstart) 1)
	     (progn
	       (setf complete t)
	       (loop-finish)))
	 (push (map 'string #'code-char (subseq rxbuf rxstart (1- i))) lines)
	 (setf rxstart (1+ i) rxpos (1+ i))
       finally
	 (setf rxpos i)
	 (return complete))))

(defun try-parse-old (req)
  (with-slots (rxstart rxpos rxlen rxbuf request-line header-lines) req
    (loop for i from rxpos below rxlen
       if
	 (and
	     (= (aref rxbuf i) (char-code #\Newline))
	     (and (> i 0) (= (aref rxbuf (1- i)) (char-code #\return)))
	     (null request-line))
       do
	 (setf request-line (map 'string #'code-char (subseq rxbuf rxstart (1- i)))
	       rxstart (1+ i)
	       rxpos (1+ i))
       else if (and
	     (= (aref rxbuf i) (char-code #\Newline))
	     (and (> i 0) (= (aref rxbuf (1- i)) (char-code #\return)))
	     (not (null request-line)))
       do
	 (push (map 'string #'code-char (subseq rxbuf rxstart (1- i))) header-lines)
	 (setf rxpos (1+ i) rxstart (1+ i))
       finally
	 (setf rxpos i))))
