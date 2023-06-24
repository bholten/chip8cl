LISP ?= sbcl

build:
	$(LISP) --load chip8cl.asd \
		--eval '(ql:quickload "chip8cl")' \
		--eval '(asdf:make "chip8cl")' \
		--eval '(quit)'
