(in-package :decent)

(defstruct http-request
  (method nil)
  (uri nil)
  (proto nil)
  (headers (make-hash-table :test 'string= :synchronized t)))

(defun process-request (req)
  '(200 (("Host" . "localhost:9090")
	 ("Content-Length" . 46)
	 ("Content-Type" . "text/html")
	 ("Connection" . "close"))

    "<html><head><title>title</title></head></html>"))
