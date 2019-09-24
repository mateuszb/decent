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
   :make-router)

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
