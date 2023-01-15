;;;; pax.asd

(asdf:defsystem  :pac
  :description "Describe pax here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :pathname "./src"
  :depends-on (#:series
               #:fiveam
               )
  :components ((:file "package")
               (:file "pac")
               )
  :in-order-to ((asdf:test-op (asdf:test-op "pac/tests")))
  )

(asdf:defsystem "pac/tests" 
  :version "0.0.1"
  :serial t
  :pathname "/pac/tests"
  :depends-on (#:pac
               #:fiveam)
  :components ((:file "main"))
  :description "TESTS"
  :perform (asdf:test-op (op c) (symbol-call :5am :run! c))
  )

;;(asdf:test-system :pac/test)



