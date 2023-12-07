(asdf:defsystem #:advent
  :description "Describe advent here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:str #:arrows #:fiveam)
  :components ((:file "package")
               (:file "advent")
               (:file "day1")
               (:file "day2")
               (:file "day3")
               (:file "day4")))
