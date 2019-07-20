(in-package :cl-user)

(defpackage decent/test
  (:use :cl :prove :decent))

(in-package :decent/test)
(plan 2)

(let ((ringbuf (decent::make-ringbuffer :read 4090 :write 4095)))
  (with-slots (read write) ringbuf
    ;;(decent::consume ringbuf 1)
    (is (decent::ring-buffer-size ringbuf) 5)
    (decent::advance ringbuf 10)
    (is (decent::ring-buffer-size ringbuf) 15)
    (format t "read=~a, write=~a~%" read write)))

(finalize)
