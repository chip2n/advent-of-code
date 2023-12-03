(asdf:defsystem #:advent
  :description "Describe advent here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:fiveam)
  :components ((:file "package")
               (:file "advent")))
