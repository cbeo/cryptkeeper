;;;; cryptkeeper.asd

(asdf:defsystem #:cryptkeeper
  :description "Describe cryptkeeper here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:trivial-irc #:alexandria  #:bknr.datastore)
  :components ((:file "cryptkeeper")))
