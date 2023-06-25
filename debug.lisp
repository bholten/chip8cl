(in-package #:chip8cl)

;; This is just debug functionality for local development,
;; live-coding, etc.

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
      (load-rom  "./roms/SYZYGY" c))
    c))

;; This starts a CHIP-8 emulator on another thread
;;
;; It won't lock up the repl and you can make updates while the
;; program is running, and see the changes live.
;;
;; It can also be useful to edit or comment-out the update function in
;; the ui.lisp file, then you can manually advance each instruction on
;; the emulator
(defun test ()
  (livesupport:setup-lisp-repl)
  (bordeaux-threads:make-thread (lambda ()
				  (make-window *test-chip8*)) :name "main-thread"))



