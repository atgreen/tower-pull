;;;; tower-pull.asd

(asdf:defsystem #:tower-pull
  :description "Pull job data from a Tower server in csv form"
  :author "Anthony Green <green@redhat.com>"
  :license "AGPL3"
  :version "1.0.0"
  :serial t
  :components ((:file "package")
               (:file "tower-pull"))
  :depends-on (:cl-json :str :drakma :flexi-streams :local-time :chronicity :clingon :cl-date-time-parser)
  :build-operation "program-op"
  :build-pathname "tower-pull"
  :entry-point "tower-pull:main")
