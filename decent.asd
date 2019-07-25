(defsystem "decent"
  :version "0.1.0"
  :author "Mateusz Berezecki"
  :license "BSD"
  :depends-on ("socket"
	       "reactor"
	       "string-case"
	       "cl-speedy-queue"
	       "spinneret"
	       "socket")
  :components ((:file "packages")
	       (:file "request" :depends-on ("packages"))
	       (:file "parse" :depends-on ("packages" "request"))
	       (:file "connection" :depends-on ("packages"))
	       (:file "server" :depends-on ("packages" "parse" "connection")))
  :in-order-to ((test-op (test-op "decent/test")))
  :description "a small http web server")

(defsystem "decent/test"
  :depends-on ("prove")
  :defsystem-depends-on (:prove-asdf)
  :serial t
  :components ((:module "tests" :components ((:test-file "test"))))
  :perform (test-op :after (o c)
		    (funcall (intern #.(string :run) :prove) c)))
