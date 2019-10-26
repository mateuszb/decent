(in-package #:decent.test)

(def-suite all-tests :description "Test suite")

(in-suite all-tests)

(defun test-decent ()
  (run! 'all-tests))

(defun front-page1 ()
  '(200 ("Content-Length" . 0) ""))

(defun front-page2 ()
  '(200 ("Content-Length" . 4) "zomg"))

(defun static-file (subdir fname)
  (let ((response-str (format nil "/~a/~a" subdir fname)))
    (list 200 (cons "Content-Length" (length response-str)) response-str)))

(test test/scopes-with-routes
  (let* ((*app*
	  (make-app 8443
	    (with-scope (host "test.com")
	      (:get "/" front-page1)
	      (:get "/(css|img|js)/(.*)" static-file))

	    (with-scope (host "example.com")
	      (:get "/" front-page2))))
	 (headers1 (let ((ht (make-hash-table)))
		     (loop for (k . v) in '((:host . "test.com"))
			do (setf (gethash k ht) v))
		     ht))
	 (headers2 (let ((ht (make-hash-table)))
		     (loop for (k . v) in '((:host . "example.com"))
			do (setf (gethash k ht) v))
		     ht))
	 (test-request1 (make-http-request :method :get :uri "/" :headers headers1))
	 (test-request2 (make-http-request :method :get :uri "/" :headers headers2))
	 (test-request3 (make-http-request :method :post :uri "/signup" :headers headers1))
	 (test-request4 (make-http-request :method :get :uri "/img/test.png" :headers headers1)))

    (is-true (string= (caddr (app-handler test-request1)) ""))
    (is-true (string= (caddr (app-handler test-request2)) "zomg"))
    (is-true (null (app-handler test-request3)))
    (is-true (string= (caddr (app-handler test-request4)) "/img/test.png"))))
