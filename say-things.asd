;;;; say-things.asd

(asdf:defsystem #:say-things
  :description "Describe say-things here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop #:plokami)
  :components ((:file "package")
               (:file "dictionary")
               (:file "say-things")))
