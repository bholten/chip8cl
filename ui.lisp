(in-package #:chip8cl)

(defun draw (ren cpu)
  (sdl2:set-render-draw-color ren 50 50 50 0)
  (sdl2:render-clear ren)
  (sdl2:set-render-draw-color ren 0 255 255 0)
  (with-slots (display-buffer) cpu
    (destructuring-bind (x y) (array-dimensions display-buffer)
      (loop :for i :below x
	    :do (loop :for j :below y
		      :for elem = (aref display-buffer i j)
		      :when elem
		      :do (sdl2:render-fill-rect ren (sdl2:make-rect (* i 10) (* j 10) 10 10)))))
    (sdl2:render-present ren)))

(defun make-window (cpu)
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "CHIP-8" :w 640 :h 320)
      (sdl2:with-renderer (ren win)
	(sdl2:with-event-loop (:method :poll)
	  (:quit () t)
	  (:idle ()
		 #+debug
		 (livesupport:update-repl-link)
		 #-debug
		 (update cpu)
		 (draw ren cpu)
		 (sdl2:delay 16)))))))

(defun update (cpu)
  (tick cpu))

