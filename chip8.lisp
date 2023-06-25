(in-package #:chip8cl)

;;
;; Most references from:
;; evernay.free.fr/hacks/chip8/C8TECH10.HTM
;;
;; Note: not 100% accurate
;;
;; CHIP-8 CPU
;;
;; The Chip-8 language is capable of accessing up to 4KB (4,096 bytes)
;; of RAM, from location 0x000 (0) to 0xFFF (4095). The first 512
;; bytes, from 0x000 to 0x1FF, are where the original interpreter was
;; located, and should not be used by programs.
;;
;; Most Chip-8 programs start at location 0x200 (512), but some begin
;; at 0x600 (1536). Programs beginning at 0x600 are intended for the
;; ETI 660 computer.
;;
;; Memory Map:
;; +---------------+= 0xFFF (4095) End of Chip-8 RAM
;; |               |
;; |               |
;; |               |
;; |               |
;; |               |
;; | 0x200 to 0xFFF|
;; |     Chip-8    |
;; | Program / Data|
;; |     Space     |
;; |               |
;; |               |
;; |               |
;; +- - - - - - - -+= 0x600 (1536) Start of ETI 660 Chip-8 programs
;; |               |
;; |               |
;; |               |
;; +---------------+= 0x200 (512) Start of most Chip-8 programs
;; | 0x000 to 0x1FF|
;; | Reserved for  |
;; |  interpreter  |
;; +---------------+= 0x000 (0) Start of Chip-8 RAM
;;
;;
;;
;; REGISTERS
;;
;;
;; Chip-8 has 16 general purpose 8-bit registers, usually referred to
;; as Vx, where x is a hexadecimal digit (0 through F). There is also
;; a 16-bit register called I. This register is generally used to
;; store memory addresses, so only the lowest (rightmost) 12 bits are
;; usually used.
;;
;; The VF register should not be used by any program, as it is used as
;; a flag by some instructions. See section 3.0, Instructions for
;; details.
;;
;; Chip-8 also has two special purpose 8-bit registers, for the delay
;; and sound timers. When these registers are non-zero, they are
;; automatically decremented at a rate of 60Hz. See the section 2.5,
;; Timers & Sound, for more information on these.
;;
;; There are also some "pseudo-registers" which are not accessable
;; from Chip-8 programs. The program counter (PC) should be 16-bit,
;; and is used to store the currently executing address. The stack
;; pointer (SP) can be 8-bit, it is used to point to the topmost level
;; of the stack.
;;
;; The stack is an array of 16 16-bit values, used to store the
;; address that the interpreter shoud return to when finished with a
;; subroutine. Chip-8 allows for up to 16 levels of nested
;; subroutines.
;;
;;
;;
;; KEYBOARD
;;
;;
;; The computers which originally used the Chip-8 Language had a
;; 16-key hexadecimal keypad with the following
;;
;; 1	2	3	C 
;; 4	5	6	D 
;; 7	8	9	E 
;; A	0	B	F 
;;
;; This layout must be mapped into various other configurations to fit
;; the keyboards of today's platforms.
;;
;;
;;
;;
;; DISPLAY
;;
;;
;; The original implementation of the Chip-8 language used a
;; 64x32-pixel monochrome display with this format:
;;
;; (0,0)	(63,0)
;; (0,31)	(63,31)
;;
;; Some other interpreters, most notably the one on the ETI 660, also
;; had 64x48 and 64x64 modes. To my knowledge, no current interpreter
;; supports these modes. More recently, Super Chip-48, an interpreter
;; for the HP48 calculator, added a 128x64-pixel mode. This mode is
;; now supported by most of the interpreters on other platforms.
;;
;; Chip-8 draws graphics on screen through the use of sprites. A
;; sprite is a group of bytes which are a binary representation of the
;; desired picture. Chip-8 sprites may be up to 15 bytes, for a
;; possible sprite size of 8x15.
;;
;; Programs may also refer to a group of sprites representing the
;; hexadecimal digits 0 through F. These sprites are 5 bytes long, or
;; 8x5 pixels. The data should be stored in the interpreter area of
;; Chip-8 memory (0x000 to 0x1FF). Below is a listing of each
;; character's bytes, in binary and hexadecimal:
;;
;; (OMITTED)
;;
;;
;;
;; TIMERS AND SOUND
;;
;; 
;; Chip-8 provides 2 timers, a delay timer and a sound timer.
;;
;; The delay timer is active whenever the delay timer register (DT) is
;; non-zero. This timer does nothing more than subtract 1 from the
;; value of DT at a rate of 60Hz. When DT reaches 0, it deactivates.
;;
;; The sound timer is active whenever the sound timer register (ST) is
;; non-zero. This timer also decrements at a rate of 60Hz, however, as
;; long as ST's value is greater than zero, the Chip-8 buzzer will
;; sound. When ST reaches zero, the sound timer deactivates.
;;
;; The sound produced by the Chip-8 interpreter has only one tone. The
;; frequency of this tone is decided by the author of the interpreter.
;;

(defvar *sp*
  (list #xF0 #x90 #x90 #x90 #xF0
	#x20 #x60 #x20 #x20 #x70
	#xF0 #x10 #xF0 #x80 #xF0
	#xF0 #x10 #xF0 #x10 #xF0
	#x90 #x90 #xF0 #x10 #x10
	#xF0 #x80 #xF0 #x10 #xF0
	#xF0 #x80 #xF0 #x90 #xF0
	#xF0 #x10 #x20 #x40 #x40
	#xF0 #x90 #xF0 #x90 #xF0
	#xF0 #x90 #xF0 #x10 #xF0
	#xF0 #x90 #xF0 #x90 #x90
	#xE0 #x90 #xE0 #x90 #xE0
	#xF0 #x80 #x80 #x80 #xF0
	#xF0 #x90 #x90 #x90 #xE0
	#xF0 #x80 #xF0 #x80 #xF0
	#xF0 #x80 #xF0 #x80 #x80))

(defun make-memory ()
  (let ((arr (make-array '(4096) :initial-element #x00
				 :adjustable nil
				 :element-type '(unsigned-byte 8))))
    (loop :for i :from 0 :below (length *sp*)
	  :for elem = (nth i *sp*)
	  :do (setf (aref arr i) elem))
    arr))

(defclass cpu ()
  ((display-buffer
    :initarg :display-buffer
    :accessor cpu-display-buffer
    :initform (make-array '(64 32) :initial-element nil :adjustable nil))
   (keyboard-buffer
    :initarg :keyboard-buffer
    :accessor cpu-keyboard-buffer
    :initform (make-array '(16) :initial-element nil :adjustable nil))
   (memory
    :initarg :memory
    :accessor cpu-memory
    :initform (make-memory))
   (registers
    :initarg :registers
    :accessor cpu-registers
    :initform (make-array '(16) :initial-element #x00 :adjustable nil :element-type '(unsigned-byte 8)))
   (stack
    :initarg :stack
    :accessor cpu-stack
    :initform (make-array '(16) :initial-element #x0000 :adjustable nil :element-type '(unsigned-byte 16)))
   (i
    :initarg :i
    :accessor cpu-i
    :initform #x0000
    :type (unsigned-byte 16))
   (pc
    :initarg :pc
    :accessor cpu-pc
    :initform #x0200
    :type (unsigned-byte 16))
   (sp
    :initarg :sp
    :accessor cpu-sp
    :initform #x0000
    :type (unsigned-byte 8))
   (delay-timer
    :initarg :delay-timer
    :accessor cpu-delay-timer
    :initform #x00
    :type (unsigned-byte 8))
   (sound-timer
    :initarg :sound-timer
    :accessor cpu-sound-timer
    :initform #x00
    :type (unsigned-byte 8))))

(defun key-pressed? (keyboard-buffer keycode)
  (aref keyboard-buffer keycode))

(defun sdl-scancode->chip8-key! (cpu scancode)
  (with-slots (keyboard-buffer) cpu
    (let ((key (cond
		 ((sdl2:scancode= scancode :scancode-1) #x1)
		 ((sdl2:scancode= scancode :scancode-2) #x2)
		 ((sdl2:scancode= scancode :scancode-3) #x3)
		 ((sdl2:scancode= scancode :scancode-4) #xC)
		 ((sdl2:scancode= scancode :scancode-q) #x4)
		 ((sdl2:scancode= scancode :scancode-w) #x5)
		 ((sdl2:scancode= scancode :scancode-e) #x6)
		 ((sdl2:scancode= scancode :scancode-r) #xD)
		 ((sdl2:scancode= scancode :scancode-a) #x7)
		 ((sdl2:scancode= scancode :scancode-s) #x8)
		 ((sdl2:scancode= scancode :scancode-d) #x9)
		 ((sdl2:scancode= scancode :scancode-f) #xE)
		 ((sdl2:scancode= scancode :scancode-z) #xA)
		 ((sdl2:scancode= scancode :scancode-x) #x0)
		 ((sdl2:scancode= scancode :scancode-c) #xB)
		 ((sdl2:scancode= scancode :scancode-v) #xF)
		 (t nil))))
      (when key
	(setf (aref keyboard-buffer key) (not (aref keyboard-buffer key)))))))

(defun add-u8 (n m)
  (mod (+ n m) 256))

(defun sub-u8 (n m)
  (mod (- n m) 256))

(defun load-rom (file cpu)
  (with-slots (memory) cpu
    (with-open-file (stream file
			    :direction :input
			    :element-type '(unsigned-byte 8))
		    (loop :for i :from #x200
			  :for byte = (read-byte stream nil nil)
			  :while byte
			  :do (setf (aref memory i) byte)))))

(defclass cycle () ())
(defclass next (cycle) ())
(defclass skip (cycle) ())
(defclass jump (cycle) ((nnn :initarg :nnn :reader jump-nnn)))

;; Crazy idea: make this execute-instruction
(defgeneric next-cycle (cpu cycle))

(defmethod next-cycle ((cpu cpu) (next next))
  (with-slots (pc) cpu
    (setf pc (+ 2 pc))
    cpu))

(defmethod next-cycle ((cpu cpu) (skip skip))
  (with-slots (pc) cpu
    (setf pc (+ 4 pc))
    cpu))

(defmethod next-cycle ((cpu cpu) (jump jump))
  (with-slots (pc) cpu
    (with-slots (nnn) jump
      (setf pc nnn)
      cpu)))

(defvar *next* (make-instance 'next))
(defvar *skip* (make-instance 'skip))

(defclass instruction () ())

(defgeneric execute-instruction (chip8 inst))

;; Convienence macro for making instructions
;;
;; Example:
;;
;; (definstruction foo aa bb)
;;
;; Expands to:
;;
;; (defclass foo (instruction)
;;   ((aa :initarg :aa :reader foo-aa) (bb :initarg :bb :reader foo-bb)))
;;
;; It also generates a print-object method:
;; 
;; (defmethod print-object ((obj foo) out)
;;   (print-unreadable-object (obj out :type t :identity t)
;;     (dolist (slot '(aa bb))
;;       (format out "~A: ~X " (string slot) (slot-value obj slot))))))
;;
(defmacro definstruction (name &rest fields)
  `(progn
     (defclass ,name (instruction)
       ,(loop :for field :in fields
	      :collect `(,field :initarg ,(intern (symbol-name field) "KEYWORD")
				:reader ,(intern (concatenate 'string (symbol-name name) "-" (symbol-name field))))))
     (defmethod print-object ((obj ,name) out)
       (print-unreadable-object (obj out :type t :identity t)
	 (dolist (slot ',fields)
	   (format out "~A: ~X "
		   (string slot)
		   (slot-value obj slot)))))))

;; 0nnn - SYS addr
;;
;; Jump to a machine code routine at nnn.
;;
;; This instruction is only used on the old computers on which Chip-8
;; was originally implemented. It is ignored by modern interpreters.
(definstruction sys-addr nnn)

(defmethod execute-instruction ((cpu cpu) (sys-addr sys-addr))
  (with-slots (pc) cpu
    (with-slots (nnn) sys-addr
      (setf pc nnn)
      *next*)))

;; 00E0 - CLS
;;
;; Clear the display.
;;
(definstruction cls)

(defmethod execute-instruction ((cpu cpu) (cls cls))
  (with-slots (display-buffer) cpu
    (dotimes (i 64)
      (dotimes (j 32)
	(setf (aref display-buffer i j) nil)))
    *next*))

;; 00EE - RET
;;
;; Return from a subroutine.
;;
;; The interpreter sets the program counter to the address at the top of
;; the stack, then subtracts 1 from the stack pointer.
(definstruction ret)

(defmethod execute-instruction ((cpu cpu) (ret ret))
  (with-slots (pc sp stack) cpu
    (decf sp)
    (setf pc (aref stack sp))
    *next*))

;; 1nnn - JP addr
;;
;; Jump to location nnn.
;;
;; The interpreter sets the program counter to nnn.
(definstruction jp-addr nnn)

(defmethod execute-instruction ((cpu cpu) (jp-addr jp-addr))
  (with-slots (nnn) jp-addr
    (make-instance 'jump :nnn nnn)))

;; 2nnn - CALL addr
;;
;; Call subroutine at nnn.
;;
;; The interpreter increments the stack pointer, then puts the current
;; PC on the top of the stack. The PC is then set to nnn.
(definstruction call-addr nnn)

(defmethod execute-instruction ((cpu cpu) (call-addr call-addr))
  (with-slots (stack sp pc) cpu
    (with-slots (nnn) call-addr
      (setf (aref stack sp) pc)
      (incf sp)
      (make-instance 'jump :nnn nnn))))

;; 3xkk - SE Vx, byte
;;
;; Skip next instruction if Vx = kk.
;;
;; The interpreter compares register Vx to kk, and if they are
;; equal, increments the program counter by 2.
(definstruction se-vx-nn x nn)

(defmethod execute-instruction ((cpu cpu) (se-vx-nn se-vx-nn))
  (with-slots (registers) cpu
    (with-slots (x nn) se-vx-nn
      (let ((vx (aref registers x)))
	(if (equal vx nn)
	    *skip*
	    *next*)))))

;; 4xkk - SNE Vx, byte
;;
;; Skip next instruction if Vx != kk.
;;
;; The interpreter compares register Vx to kk, and if they are not
;; equal, increments the program counter by 2.
(definstruction sne-vx-nn x nn)

(defmethod execute-instruction ((cpu cpu) (sne-vx-nn sne-vx-nn))
  (with-slots (registers) cpu
    (with-slots (x nn) sne-vx-nn
      (let ((vx (aref registers x)))
	(if (not (equal vx nn))
	    *skip*
	    *next*)))))

;; 5xy0 - SE Vx,
;;
;; Skip next instruction if Vx = Vy.
;;
;; The interpreter compares register Vx to register Vy, and if they
;; are equal, increments the program counter by 2.
(definstruction se-vx-vy x y)

(defmethod execute-instruction ((cpu cpu) (se-vx-vy se-vx-vy))
  (with-slots (registers) cpu
    (with-slots (x y) se-vx-vy
      (let ((vx (aref registers x))
	    (vy (aref registers y)))
	(if (equal vx vy)
	    *skip*
	    *next*)))))

;; 6xkk - LD Vx, byte
;;
;; Set Vx = kk.
;;
;; The interpreter puts the value kk into register Vx.
(definstruction ld-vx-nn x nn)

(defmethod execute-instruction ((cpu cpu) (ld-vx-nn ld-vx-nn))
  (with-slots (registers) cpu
    (with-slots (x nn) ld-vx-nn
      (setf (aref registers x) nn) ; TODO mod necessary?
      *next*)))

;; 7xkk - ADD Vx, byte
;;
;; Set Vx = Vx + kk.
;;
;; Adds the value kk to the value of register Vx, then stores the
;; result in Vx.
(definstruction add-vx-nn x nn)

(defmethod execute-instruction ((cpu cpu) (add-vx-nn add-vx-nn))
  (with-slots (registers) cpu
    (with-slots (x nn) add-vx-nn
      (let ((vx (aref registers x)))
	(setf (aref registers x)  (add-u8 vx nn))
	*next*))))

;; 8xy0 - LD Vx, Vy
;;
;; Set Vx = Vy.
;;
;; Stores the value of register Vy in register Vx.
(definstruction ld-vx-vy x y)

(defmethod execute-instruction ((cpu cpu) (ld-vx-vy ld-vx-vy))
  (with-slots (registers) cpu
    (with-slots (x y) ld-vx-vy
      (let ((vy (aref registers y)))
	(setf (aref registers x) vy)
	*next*))))

;; 8xy1 - OR Vx, Vy
;;
;; Set Vx = Vx OR Vy.
;;
;; Performs a bitwise OR on the values of Vx and Vy, then stores the
;; result in Vx. A bitwise OR compares the corrseponding bits from two
;; values, and if either bit is 1, then the same bit in the result is
;; also 1. Otherwise, it is 0.
(definstruction or-vx-vy x y)

(defmethod execute-instruction ((cpu cpu) (or-vx-vy or-vx-vy))
  (with-slots (registers) cpu
    (with-slots (x y) or-vx-vy
      (let* ((vx (aref registers x))
	     (vy (aref registers y))
	     (result (logior vx vy)))
	(setf (aref registers x) result)
	*next*))))

;; 8xy2 - AND Vx,
;;
;; Set Vx = Vx AND Vy.
;;
;; Performs a bitwise AND on the values of Vx and Vy, then stores the
;; result in Vx. A bitwise AND compares the corrseponding bits from two
;; values, and if both bits are 1, then the same bit in the result is
;; also 1. Otherwise, it is 0.
(definstruction and-vx-vy x y)

(defmethod execute-instruction ((cpu cpu) (and-vx-vy and-vx-vy))
  (with-slots (registers) cpu
    (with-slots (x y) and-vx-vy
      (let* ((vx (aref registers x))
	     (vy (aref registers y))
	     (result (logand vx vy)))
	(setf (aref registers x) result)
	*next*))))

;; 8xy3 - XOR Vx, Vy
;;
;; Set Vx = Vx XOR Vy.
;;
;; Performs a bitwise exclusive OR on the values of Vx and Vy, then
;; stores the result in Vx. An exclusive OR compares the corrseponding
;; bits from two values, and if the bits are not both the same, then
;; the corresponding bit in the result is set to 1. Otherwise, it is
;; 0.
(definstruction xor-vx-vy x y)

(defmethod execute-instruction ((cpu cpu) (xor-vx-vy xor-vx-vy))
  (with-slots (registers) cpu
    (with-slots (x y) xor-vx-vy
      (let* ((vx (aref registers x))
	     (vy (aref registers y))
	     (result (logxor vx vy)))
	(setf (aref registers x) result)
	*next*))))

;; 8xy4 - ADD Vx,
;;
;; Set Vx = Vx + Vy, set VF = carry.
;;
;; The values of Vx and Vy are added together. If the result is greater
;; than 8 bits (i.e., > 255,) VF is set to 1, otherwise 0. Only the
;; lowest 8 bits of the result are kept, and stored in Vx.
(definstruction add-vx-vy-vf x y)

(defmethod execute-instruction ((cpu cpu) (opcode add-vx-vy-vf))
  (with-slots (registers) cpu
    (with-slots (x y) opcode
      (let* ((vx (aref registers x))
	     (vy (aref registers y))
	     (sum (+ vx vy)))
	(setf (aref registers #x0F) (if (> sum #xFF) 1 0))
	(setf (aref registers x) (mod sum 256))
	*next*))))

;; 8xy5 - SUB Vx, Vy
;;
;; Set Vx = Vx - Vy, set VF = NOT borrow.
;;
;; If Vx > Vy, then VF is set to 1, otherwise 0. Then Vy is subtracted
;; from Vx, and the results stored in Vx.
(definstruction sub-vx-vy-vf x y)

(defmethod execute-instruction ((cpu cpu) (sub-vx-vy-vf sub-vx-vy-vf))
  (with-slots (registers) cpu
    (with-slots (x y) sub-vx-vy-vf
      (let* ((vx (aref registers x))
	     (vy (aref registers y))
	     (sub (sub-u8 vx vy)))
	(setf (aref registers x) sub)
	(setf (aref registers #x0F) (if (> vx vy) 1 0))
	*next*))))

;; 8xy6 - SHR Vx {, Vy}
;;
;; Set Vx = Vx SHR 1.
;;
;; If the least-significant bit of Vx is 1, then VF is set to 1,
;; otherwise 0. Then Vx is divided by 2.
(definstruction shr-vx-vf x y)

(defmethod execute-instruction ((cpu cpu) (shr-vx-vf shr-vx-vf))
  (with-slots (registers) cpu
    (with-slots (x y) shr-vx-vf
      (let* ((vy (aref registers y))
	     (ys (ash vy -1))
	     (lsb (logand vy #b00000001)))
	(setf (aref registers x) ys)
	(setf (aref registers #x0F) (if (equal lsb 1) 1 0))
	*next*))))

;; 8xy7 - SUBN Vx, Vy
;;
;; Set Vx = Vy - Vx, set VF = NOT borrow.
;;
;; If Vy > Vx, then VF is set to 1, otherwise 0. Then Vx is subtracted
;; from Vy, and the results stored in Vx.
(definstruction subn-vx-vy-vf x y)

(defmethod execute-instruction ((cpu cpu) (subn-vx-vy-vf subn-vx-vy-vf))
  (with-slots (registers) cpu
    (with-slots (x y) subn-vx-vy-vf
      (let ((vx (aref registers x))
	    (vy (aref registers y)))
	(setf (aref registers x) (sub-u8 vy vx))
	(setf (aref registers #x0F) (if (> vy vx) 1 0))
	*next*))))

;; 8xyE - SHL Vx {, Vy}
;;
;; Set Vx = Vx SHL 1.
;;
;; If the most-significant bit of Vx is 1, then VF is set to 1, otherwise
;; to 0. Then Vx is multiplied by 2.
(definstruction shl-vx-vf x y)

;; TODO lots of conflicting information on this one
;; https://chip8.gulrak.net/reference/opcodes/#quirk5
;; https://tobiasvl.github.io/blog/write-a-chip-8-emulator/#instructions
;; https://github.com/bholten/chip8ml/blob/master/lib/cpu.ml
(defmethod execute-instruction ((cpu cpu) (shl-vx-vf shl-vx-vf))
  (with-slots (registers) cpu
    (with-slots (x y) shl-vx-vf
      (let* ((vy (aref registers y))
	     (vvy (ash vy 1))
	     (msb (logand vy #b10000000)))
	(setf (aref registers x) (mod vvy #xFF))
	(setf (aref registers #x0F) (if (zerop msb) 0 1))
	*next*))))

;; 9xy0 - SNE Vx, Vy
;;
;; Skip next instruction if Vx != Vy.
;;
;; The values of Vx and Vy are compared, and if they are not equal,
;; the program counter is increased by 2.
(definstruction sne-vx-vy-vf x y)

(defmethod execute-instruction ((cpu cpu) (sne-vx-vy-vf sne-vx-vy-vf))
  (with-slots (registers) cpu
    (with-slots (x y) sne-vx-vy-vf
      (let ((vx (aref registers x))
	    (vy (aref registers y)))
	(if (not (equal vx vy))
	    *skip*
	    *next*)))))

;; Annn - LD I, addr
;;
;; Set I = nnn.
;;
;; The value of register I is set to nnn.
(definstruction ld-i-addr nnn)

(defmethod execute-instruction ((cpu cpu) (ld-i-addr ld-i-addr))
  (with-slots (i) cpu
    (with-slots (nnn) ld-i-addr
      (setf i nnn)
      *next*)))

;; Bnnn - JP V0, addr
;;
;; Jump to location nnn + V0.
;;
;; The program counter is set to nnn plus the value of V0.
(definstruction jp-v0-addr nnn)

(defmethod execute-instruction ((cpu cpu) (jp-v0-addr jp-v0-addr))
  (with-slots (registers) cpu
    (with-slots (nnn) jp-v0-addr
      (let ((v0 (aref registers 0)))
	(make-instance 'jump :nnn (+ v0 nnn))))))

;; Cxkk - RND Vx, byte
;;
;; Set Vx = random byte AND kk.
;;
;; The interpreter generates a random number from 0 to 255, which is
;; then ANDed with the value kk. The results are stored in Vx. See
;; instruction 8xy2 for more information on AND.
(definstruction rnd-vx-nn x nn)

(defmethod execute-instruction ((cpu cpu) (opcode rnd-vx-nn))
  (with-slots (registers) cpu
    (with-slots (x nn) opcode
      (let ((r (logand (random 255) nn)))
	(setf (aref registers x) r)
	*next*))))

;; Dxyn - DRW Vx, Vy, nibble
;;
;; Display n-byte sprite starting at memory location I at (Vx, Vy),
;; set VF = collision.
;;
;; The interpreter reads n bytes from memory, starting at the address
;; stored in I. These bytes are then displayed as sprites on screen at
;; coordinates (Vx, Vy). Sprites are XORed onto the existing
;; screen. If this causes any pixels to be erased, VF is set to 1,
;; otherwise it is set to 0. If the sprite is positioned so part of it
;; is outside the coordinates of the display, it wraps around to the
;; opposite side of the screen. See instruction 8xy3 for more
;; information on XOR, and section 2.4, Display, for more information
;; on the Chip-8 screen and sprites.
(definstruction drw-vx-vy-n x y n)

(defmethod execute-instruction ((cpu cpu) (opcode drw-vx-vy-n))
  (with-slots (display-buffer memory registers i) cpu
    (with-slots (x y n) opcode
      (let* ((vx (aref registers x))
	     (vy (aref registers y)))
	(setf (aref registers #x0F) 0)
	(loop :for a :from 0 :below n
	      :do (let ((sprite (aref memory (+ a i)))
			(dy (mod (+ a vy) 32)))
		    (loop :for b :from 0 :below 8
			  :do (let* ((bmsb (ash #b10000000 (- 0 b)))
				     (s (logand sprite bmsb)))
				(when (not (zerop s))
				  (let* ((dx (mod (+ b vx) 64))
					 (set (aref display-buffer dx dy)))
				    (when set (setf (aref registers #x0F) 1))
				    (setf (aref display-buffer dx dy) (not set))))))))
	*next*))))

;; Ex9E - SKP Vx
;;
;; Skip next instruction if key with the value of Vx is pressed.
;;
;; Checks the keyboard, and if the key corresponding to the value of
;; Vx is currently in the down position, PC is increased by 2.
(definstruction skp-vx x)

(defmethod execute-instruction ((cpu cpu) (opcode skp-vx))
  (with-slots (keyboard-buffer registers) cpu
      (with-slots (x) opcode
	(let ((vx (aref registers x)))
	  (if (key-pressed? keyboard-buffer vx)
	      *skip*
	      *next*)))))

;; ExA1 - SKNP Vx
;;
;; Skip next instruction if key with the value of Vx is not pressed.
;;
;; Checks the keyboard, and if the key corresponding to the value of
;; Vx is currently in the up position, PC is increased by 2.
(definstruction sknp-vx x)

(defmethod execute-instruction ((cpu cpu) (opcode sknp-vx))
  (with-slots (keyboard-buffer registers) cpu
      (with-slots (x) opcode
	(let ((vx (aref registers x)))
	  (if (not (key-pressed? keyboard-buffer vx))
	      *skip*
	      *next*)))))

;; Fx07 - LD Vx, DT
;;
;; Set Vx = delay timer value.
;;
;; The value of DT is placed into Vx.
(definstruction ld-vx-dt x)

(defmethod execute-instruction ((cpu cpu) (opcode ld-vx-dt))
  (with-slots (registers delay-timer) cpu
    (with-slots (x) opcode
      (setf (aref registers x) delay-timer)
      *next*)))

;; Fx0A - LD Vx, K
;;
;; Wait for a key press, store the value of the key in Vx.
;;
;; All execution stops until a key is pressed, then the value of that
;; key is stored in Vx.
(definstruction ld-vx-k x)

(defmethod execute-instruction ((cpu cpu) (opcode ld-vx-k))
  (with-slots (keyboard-buffer registers pc) cpu
    (with-slots (x) opcode
      (let ((result (loop :for i :from 0
			  :and k :across keyboard-buffer
			  :when k
			  :do (return (setf (aref registers x) i)))))
	(if result ; the loop returns nil if it terminates without the return hitting
	    *next*
	    (make-instance 'jump :nnn pc))))))

;; Fx15 - LD DT, Vx
;;
;; Set delay timer = Vx.
;;
;; DT is set equal to the value of Vx.
(definstruction ld-dt-vx x)

(defmethod execute-instruction ((cpu cpu) (opcode ld-dt-vx))
  (with-slots (delay-timer registers) cpu
    (with-slots (x) opcode
      (setf (aref registers x) delay-timer)
      *next*)))

;; Fx18 - LD ST, Vx
;;
;; Set sound timer = Vx.
;;
;; ST is set equal to the value of Vx.
(definstruction ld-st-vx x)

(defmethod execute-instruction ((cpu cpu) (opcode ld-st-vx))
  (with-slots (sound-timer registers) cpu
    (with-slots (x) opcode
      (setf (aref registers x) sound-timer)
      *next*)))

;; Fx1E - ADD I, Vx
;;
;; Set I = I + Vx.
;;
;; The values of I and Vx are added, and the results are stored in I.
(definstruction add-i-vx x)

(defmethod execute-instruction ((cpu cpu) (opcode add-i-vx))
  (with-slots (registers i) cpu
    (with-slots (x) opcode
      (incf i (aref registers x))
      *next*)))

;; Fx29 - LD F, Vx
;;
;; Set I = location of sprite for digit Vx.
;;
;; The value of I is set to the location for the hexadecimal sprite
;; corresponding to the value of Vx. See section 2.4, Display, for
;; more information on the Chip-8 hexadecimal font.
(definstruction ld-f-vx x)

(defmethod execute-instruction ((cpu cpu) (opcode ld-f-vx))
  (with-slots (i registers) cpu
    (with-slots (x) opcode
      (let ((vx (aref registers x)))
	(incf i (* vx 5))
	*next*))))

;; Fx33 - LD B, Vx
;;
;; Store BCD representation of Vx in memory locations I, I+1, and I+2.
;;
;; The interpreter takes the decimal value of Vx, and places the
;; hundreds digit in memory at location in I, the tens digit at
;; location I+1, and the ones digit at location I+2.
(definstruction ld-b-vx x)

;; TODO I'm not sure about the divison, too lazy to look at docs
(defun get-div-floor (num div)
  (multiple-value-bind (h i) (floor (/ num div))
    (declare (ignore i))
    h))

(defmethod execute-instruction ((cpu cpu) (opcode ld-b-vx))
  (with-slots (memory registers i) cpu
    (with-slots (x) opcode
      (let* ((vx        (aref registers x))
	     (hundreds  (floor vx 100))
	     (tens      (floor (rem vx 100) 10))
	     (ones      (rem vx 10)))
	(setf (aref memory i) hundreds)
	(setf (aref memory (+ i 1)) tens)
	(setf (aref memory (+ i 2)) ones)
	*next*))))

;; Fx55 - LD [I], Vx
;;
;; Store registers V0 through Vx in memory starting at location I.
;;
;; The interpreter copies the values of registers V0 through Vx into
;; memory, starting at the address in I.
(definstruction ld-i-vx x)

(defmethod execute-instruction ((cpu cpu) (opcode ld-i-vx))
  (with-slots (i memory registers) cpu
    (with-slots (x) opcode
      (loop :for k :from 0 :to x
	    :for kp = (+ i k)
	    :for r = (aref registers k)
	    :do (setf (aref memory kp) r))
      (incf i (+ x 1))
      *next*)))

;; Fx65 - LD Vx, [I]
;;
;; Read registers V0 through Vx from memory starting at location I.
;;
;; The interpreter reads values from memory starting at location I
;; into registers V0 through Vx.
(definstruction ld-vx-i x)

(defmethod execute-instruction ((cpu cpu) (opcode ld-vx-i))
  (with-slots (i memory registers) cpu
    (with-slots (x) opcode
      (loop :for k :from 0 :to x
	    :for m = (aref memory (+ i k))
	    :do (setf (aref registers k) m))
      (incf i (+ x i))
      *next*)))

;;
;;
(defun decode-opcode (opcode)
  (declare (type (unsigned-byte 16) opcode))
  (case opcode
    (#x00E0 (make-instance 'cls))
    (#x00EE (make-instance 'ret))
    (otherwise
     (let ((d   (logand opcode #xF000))
	   (n   (logand opcode #x000F))
	   (x   (ash (logand opcode #x0F00) -8))
	   (y   (ash (logand opcode #x00F0) -4))
	   (nn  (logand opcode #x00FF))
	   (nnn (logand opcode #x0FFF)))
       (case d
	 (#x1000 (make-instance 'jp-addr :nnn nnn))
	 (#x2000 (make-instance 'call-addr :nnn nnn))
	 (#x3000 (make-instance 'se-vx-nn :x x :nn nn))
	 (#x4000 (make-instance 'sne-vx-nn :x x :nn nn))
	 (#x5000 (make-instance 'se-vx-vy :x x :y y))
	 (#x6000 (make-instance 'ld-vx-nn :x x :nn nn))
	 (#x7000 (make-instance 'add-vx-nn :x x :nn nn))
	 (#x8000
	  (case n
	    (#x0 (make-instance 'ld-vx-vy :x x :y y))
	    (#x1 (make-instance 'or-vx-vy :x x :y y))
	    (#x2 (make-instance 'and-vx-vy :x x :y y))
	    (#x3 (make-instance 'xor-vx-vy :x x :y y))
	    (#x4 (make-instance 'add-vx-vy-vf :x x :y y))
	    (#x5 (make-instance 'sub-vx-vy-vf :x x :y y))
	    (#x6 (make-instance 'shr-vx-vf :x x :y y))
	    (#x7 (make-instance 'subn-vx-vy-vf :x x :y y))
	    (#xE (make-instance 'shl-vx-vf :x x :y y))))
	 (#x9000
	  (case n
	    (#x0 (make-instance 'sne-vx-vy-vf :x x :y y))))
	 (#xA000 (make-instance 'ld-i-addr :nnn nnn))
	 (#xB000 (make-instance 'jp-v0-addr :nnn nnn))
	 (#xC000 (make-instance 'rnd-vx-nn :x x :nn nn))
	 (#xD000 (make-instance 'drw-vx-vy-n :x x :y y :n n))
	 (#xE000
	  (case nn
	    (#x9E (make-instance 'skp-vx :x x))
	    (#xA1 (make-instance 'sknp-vx :x x))))
	 (#xF000
	  (case nn
	    (#x07 (make-instance 'ld-vx-dt :x x))
	    (#x0A (make-instance 'ld-vx-k :x x))
	    (#x15 (make-instance 'ld-dt-vx :x x))
	    (#x18 (make-instance 'ld-st-vx :x x))
	    (#x1E (make-instance 'add-i-vx :x x))
	    (#x29 (make-instance 'ld-f-vx :x x))
	    (#x33 (make-instance 'ld-b-vx :x x))
	    (#x55 (make-instance 'ld-i-vx :x x))
	    (#x65 (make-instance 'ld-vx-i :x x)))))))))

(defun get-opcode (cpu)
  (with-slots (memory pc) cpu
    (let ((byte1 (aref memory pc))
	  (byte2 (aref memory (1+ pc))))
      (logior (ash byte1 8) byte2))))

(defun tick (cpu)
  (with-slots (delay-timer sound-timer pc memory) cpu
    (let* ((opcode (get-opcode cpu))
	   (op (decode-opcode opcode))
	   (n (execute-instruction cpu op)))
      (next-cycle cpu n)
      (when (> delay-timer 0) (decf delay-timer))
      (when (> sound-timer 0) (decf sound-timer)))))
