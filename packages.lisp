(defpackage decent
  (:use :cl
	:socket
	:reactor
	:reactor.dispatch
	:string-case
	:cl-speedy-queue
	:spinneret
	:sb-sys
	:sb-alien))
