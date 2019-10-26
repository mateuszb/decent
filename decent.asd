(defsystem "decent"
  :description "a small http web server"
  :version "0.1.0"
  :author "Mateusz Berezecki"
  :license "BSD"
  :depends-on ("socket"
	       "reactor"
	       "string-case"
	       "socket"
	       "cl-ppcre"
	       "quri"
	       "tls-1.3"
	       "trivial-mimes")
  :components ((:file "packages")
	       (:file "request" :depends-on ("packages"))
	       (:file "parse" :depends-on ("packages" "request"))
	       (:file "routing" :depends-on ("packages"))
	       (:file "connection" :depends-on ("packages"))
	       (:file "server" :depends-on ("routing" "parse" "connection"))
	       (:file "app" :depends-on ("packages")))
  :in-order-to ((test-op (test-op "decent/test")))
)

(defsystem "decent/test"
  :depends-on ("decent" "fiveam")
  :components ((:module "tests"
		:serial t
		:components
		((:file "package")
		 (:file "tests" :depends-on ("package")))))
  :perform (test-op (o s)
		    (uiop:symbol-call :fiveam :run!
				      (intern #.(string :all-tests) :decent.test))))
