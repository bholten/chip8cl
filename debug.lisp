(in-package #:chip8cl)

;; In repl, to enable debug:
;;     (pushnew :debug *features*)
;; To disable:
;;     (remf *features* :debug)
#+debug
(declaim (optimize (debug 3) (safety 3) (speed 0)))
#-debug
(declaim (optimize (debug 0) (safety 0) (speed 3)))

;; Test things

(defparameter *test-chip8*
  (let ((c (make-instance 'cpu)))
    (with-slots (memory) c
      (load-rom  "./roms/4-flags.ch8" c)
      c)))

(defun test ()
  (livesupport:setup-lisp-repl)
  (bordeaux-threads:make-thread (lambda ()
				  (make-window *test-chip8*)) :name "main-thread"))



