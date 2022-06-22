;;;; say-things.asd

(asdf:defsystem #:say-things
  :description "Say things"
  :author "Ayal Lutwak <ayal@audio-electric.com>"
  :license  "Don't use this"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop #:cl-ppcre #:plokami)
  :components ((:file "package")
               (:file "dictionary")
               (:file "say-things")))
