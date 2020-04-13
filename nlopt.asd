;;;; nlopt.asd

(asdf:defsystem #:nlopt
  :description "Describe nlopt here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:cffi :trivial-package-local-nicknames)
  :components ((:file "package")
			   (:file "swig")
               (:file "nlopt")))

