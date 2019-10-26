(defpackage decent
  (:use :cl
	:socket
	:reactor
	:string-case
	:sb-sys
	:sb-alien)
  (:export
   :start
   :defroute
   :make-route
   :make-router
   :make-http-request
   :http-request-peer
   :http-request-method
   :http-request-uri
   :http-request-params
   :http-request-proto
   :http-request-body
   :http-request-headers
   :app-handler
   :make-app
   :with-scope
   :make-scope-matcher
   :route-spec->route
   :mime-type
   :*app*
   :*handler*)

  (:import-from :alien-ring
		:make-binary-ring-stream
		:close
		:make-ring-buffer
		:stream-size
		:stream-peek-char)

  (:import-from :tls
		:data
		:start-server
		:tls-read
		:tls-write
		:tls-close)

  (:import-from :flexi-streams
		:with-input-from-sequence)

  (:import-from :cl-ppcre
		:scan-to-strings)
  )
