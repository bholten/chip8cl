(in-package #:chip8cl)

(defun main ()
  (let ((file (car (uiop:command-line-arguments)))
	(chip8 (make-instance 'cpu)))
    (with-slots (memory) chip8
      (load-rom (uiop:native-namestring file) chip8)
      (make-window chip8))))
