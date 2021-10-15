(asdf:defsystem :peas-with-raylib
  :description "A game jam or nothing at all"
  :author "Rodrigo Correia <https://github.com/drigoor>"
  :version "0.0.0ervilhas"
  :depends-on (#:alexandria
               #:cl-raylib)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "main")))
