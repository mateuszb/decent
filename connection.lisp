(in-package :decent)

(defparameter +RX-BUFFER-CAPACITY+ 4096)

(defvar *connections*)
(defclass http-connection ()
  ((socket :reader socket :initform nil :initarg :socket)
   (rxbuf :type 'system-area-pointer :reader rx-buffer :initform (int-sap 0) :initarg :rxbuf)
   (rxcap :type '(unsigned-byte 32) :reader rx-capacity :initform +RX-BUFFER-CAPACITY+ :initarg :rxcap)
   (rd :type '(unsigned-byte 32) :initform 0 :initarg :rd :reader read-position)
   (wr :type '(unsigned-byte 32) :initform 0 :initarg :wr :reader write-position)
   (lines :initform '())
   (txq :initform (make-queue 16))))

(defun make-http-connection (socket)
  (make-instance 'http-connection
		 :rxbuf (make-alien (unsigned 8) +RX-BUFFER-CAPACITY+)
		 :rxcap +RX-BUFFER-CAPACITY+
		 :socket socket))

(defun release-http-connection (conn)
  (with-slots (rxbuf socket) conn
    (disconnect socket)
    (format t "freeing ~a bytes of alien buf ~a~%" +RX-BUFFER-CAPACITY+ (alien-sap rxbuf))
    (sb-alien:free-alien rxbuf)))
