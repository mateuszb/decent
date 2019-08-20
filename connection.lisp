(in-package :decent)

(defparameter +RX-BUFFER-CAPACITY+ 4096)

(defvar *connections*)
(defclass http-connection ()
  ((socket :reader socket :initform nil :initarg :socket)
   (rxbuf :type 'system-area-pointer :reader rx-buffer :initform (int-sap 0) :initarg :rxbuf)
   (rxcap :type '(unsigned-byte 32) :reader rx-capacity :initform +RX-BUFFER-CAPACITY+ :initarg :rxcap)
   (rd :type '(unsigned-byte 32) :initform 0 :initarg :rd :reader read-position)
   (rdsofar :type '(unsigned-byte 32) :initform 0 :initarg :rdsofar :reader read-so-far)
   (wr :type '(unsigned-byte 32) :initform 0 :initarg :wr :reader write-position)
   (lines :initform '())
   (request :initform nil :initarg :request)
   (txq :initform (make-queue 16))))

(defclass https-connection ()
  ((socket :reader socket :initform nil :initarg :socket)
   (lines :initform '())
   (request :initform nil :initarg :request)
   (txq :initform (make-queue 16))
   (ssl-context :initform nil :initarg :ssl-context :reader https-tls-context)
   (ssl-stream :initform nil :initarg :ssl-stream :reader https-tls-stream)))

(defun make-http-connection (socket)
  (make-instance 'http-connection
		 :rxbuf (make-alien (unsigned 8) +RX-BUFFER-CAPACITY+)
		 :rxcap +RX-BUFFER-CAPACITY+
		 :socket socket))

(defun make-https-connection (socket)
  (make-instance 'https-connection :socket socket))

(defgeneric release-connection (conn)
  (:documentation "Releases resources in a connection."))

(defmethod release-connection ((conn http-connection))
  (with-slots (rxbuf socket) conn
    (disconnect socket)
    (sb-alien:free-alien rxbuf)))

(defmethod release-connection ((conn https-connection))
  ;; TODO: fix this... for now it memory leaks
  (with-slots (socket) conn
    (disconnect socket)))

(defgeneric is-https-p (conn))
(defmethod is-https-p (conn) nil)
(defmethod is-https-p ((conn https-connection)) t)
