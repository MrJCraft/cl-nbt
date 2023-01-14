;;;; pax.asd

(asdf:defsystem  "pac"
  :description "Describe pax here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:series
               #:fiveam
               )
  :components ((:file "package")
               (:file "pac")
               )
  :in-order-to ((test-op (test-op "pac/tests")))
  )

(asdf:defsystem "pac/tests"
  :version "0.0.1"
  :serial t
  :depends-on (#:pac
               #:fiveam)
  :components ((:module "tests"
                        :components
                        ((:file "main"))))
  :description "TESTS"
  :perform (test-op (op c) (symbol-call :fiveam :run c))
  )

;;(asdf:test-system :pac/test)



