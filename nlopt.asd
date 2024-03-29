;;;; nlopt.asd

(asdf:defsystem #:nlopt
  :description "Common Lisp interface to Non-linear optimization library NLopt"
  :author "Bibek Panthi <bpanthi977@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (:cffi :trivial-garbage)
  :components ((:file "package")
	       (:file "cffi")
	       (:file "utils")
               (:file "nlopt")
	       (:file "nloptimize")))
	       

