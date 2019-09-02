(in-package :decent)

(defparameter +RX-BUFFER-CAPACITY+ 4096)

(defvar *connections*)

(defclass http-connection ()
  ((lines :initform '() :accessor request-lines)
   (request :initform nil :initarg :request :accessor http-request)
   (last-parse-pos :initform 0 :accessor last-parse-pos)))

(defclass https-connection (http-connection)
  ((tls :initarg :tls :reader tls)))

(defun make-https-connection (tls)
  (make-instance 'https-connection :tls tls))

(defgeneric release-connection (conn)
  (:documentation "Releases resources in a connection."))

(defmethod release-connection ((conn https-connection))
  ;; TODO: fix this... for now it memory leaks
  nil)

(defgeneric is-https-p (conn))
(defmethod is-https-p (conn) nil)
(defmethod is-https-p ((conn https-connection)) t)
