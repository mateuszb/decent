(defpackage decent
  (:use :cl
	:socket
	:reactor
	:reactor.dispatch
	:string-case
	:cl-speedy-queue
	:sb-sys
	:sb-alien)
  (:export
   :start
   :defroute))
