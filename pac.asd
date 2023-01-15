;;;; pax.asd

(asdf:defsystem :pac
  :description "Describe pax here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :pathname "src"
  :depends-on (#:series
               #:fiveam
               )
  :components ((:file "package")
               (:file "pac")
               )
  ;;:in-order-to ((asdf:test-op (asdf:test-op "pac/tests")))
  )

;; (asdf:defsystem "this/tests" 
;;   :version "0.0.1"
;;   :serial t
;;   :pathname "tests"
;;   :depends-on (#:series
;;                #:fiveam
;;                #:pac)
;;   :components (
;;                (:file "main")
;;                )
;;   :description "TESTS"
;;   :perform (asdf:test-op (op c) (symbol-call :fiveam :run! (find-symbol* :pac :pac/tests)))
;;   )

;;(asdf:test-system :pac/test)



