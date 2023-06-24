;;;; chip8cl.asd

(asdf:defsystem #:chip8cl
  :description "Describe chip8cl here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("livesupport"
	       "sdl2")
  :components ((:file "package")
               (:file "chip8")
	       (:file "cli")
	       (:file "ui"))
  :build-operation "program-op" ;; leave as is
  :build-pathname "chip8cl"
  :entry-point "chip8cl:main")
