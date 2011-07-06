(defstruct state (pc 0 :type integer) (registers #() :type array) (arrays #() :type array) running)

(defmacro set-reg (state n val)
  `(progn (setf (aref (state-registers ,state) ,n) ,val)
          ,state))

(defmacro nth-array (state n)
  `(aref (state-arrays ,state) ,n))

(defun set-array (state n val)
  (when (>= n (length (state-arrays state)))
    (adjust-array (state-arrays state) (1+ n)))
  (setf (aref (state-arrays state) n) val)
  state)

(defmacro nth-array-elem (state n i)
  `(aref (aref (state-arrays ,state) ,n) ,i))

(defmacro set-nth-array-elem (state n i val)
  `(progn
     (setf (aref (aref (state-arrays ,state) ,n) ,i) ,val)
     state))

;; Load code from file and return new state containing code.
(defun load-code (state file)
  (with-open-file (f file :direction :input :element-type '(unsigned-byte 8))
    (let ((code (load-data-file f)))
      (set-array state 0 (make-array (length code) :initial-contents code :element-type 'integer))
      state)))

(defun read-uint32 (f)
  (let ((c (read-byte f nil nil)))
    (if c
        (let ((i 0))
          (setf (ldb (byte 8 24) i) c
                (ldb (byte 8 16) i) (read-byte f)
                (ldb (byte 8  8) i) (read-byte f)
                (ldb (byte 8  0) i) (read-byte f))
          i)
      nil)))

;; fill in code and return it
(defun load-data-file (f)
  (loop for i = (read-uint32 f)
        while i
        collect i))

(defun instruction-at-pc (state)
  (aref (nth-array state 0) (state-pc state)))

(defun instruction-opcode (instruction)
  (boole boole-and (ash instruction -28) #x0f))

; returns a values list (a b c reg-a-val reg-b-val reg-c-val)
(defun abc-from-instruction (state instruction)
  (let* ((a (boole boole-and (ash instruction -6) 7))
         (b (boole boole-and (ash instruction -3) 7))
         (c (boole boole-and instruction          7))
         (regs (state-registers state))
         (a-val (aref regs a))
         (b-val (aref regs b))
         (c-val (aref regs c)))
    (values a b c a-val b-val c-val)))

(defun loadi-a-reg (instruction)
  (boole boole-and (ash instruction -25) 7))

(defun loadi-const (instruction)
  (boole boole-and instruction #x1ffffff))

;; Set to *out* or some other output stream if you want debug output.
(defvar *debug-out* nil)

;; Print debug message and return state. We return state so this can be the
;; last call in a function that returns state.
(defun debug-print (state msg &rest custom-out)
  (let ((*out* (or (first custom-out) *debug-out*)))
    (when *out*
      (let* ((ins (instruction-at-pc state))
             (opcode (ash ins -28)))
        (multiple-value-bind (a b c ra rb rc) (abc-from-instruction state ins)
          (if (= opcode 13)
              (format t "~8x loadi    ~d, ~d   " (state-pc state) (loadi-a-reg ins) (loadi-const ins))
            (format t "~8x ~8a ~d, ~d, ~d" (state-pc state) msg a b c))
          (format t "~8t; regs = ~a~%" (state-registers state)))))))

;; move
;; The register A receives the value in register B, unless the register C
;; contains 0.
(defun vmi-move (state instruction)
  (debug-print state "move")
  (multiple-value-bind (a b c ra rb rc) (abc-from-instruction state instruction)
    (if (zerop rc)
      state
      (set-reg state a rb))))

;; array index
;; The register A receives the value stored at offset in register C in the
;; array identified by B.
(defun vmi-array-index (state instruction)
  (debug-print state "aindex")
  (multiple-value-bind (a b c ra rb rc) (abc-from-instruction state instruction)
    (set-reg state a (nth-array-elem state rb rc))))

;; array amendment
;; The array identified by A is amended at the offset in register B to store
;; the value in register C.
(defun vmi-array-amendment (state instruction)
  (debug-print state "aamend")
  (multiple-value-bind (a b c ra rb rc) (abc-from-instruction state instruction)
    (set-nth-array-elem state ra rb rc)))

;; addition
;; The register A receives the value in register B plus the value in
;; register C, modulo 2^32.
(defun vmi-addition (state instruction)
  (debug-print state "add")
  (multiple-value-bind (a b c ra rb rc) (abc-from-instruction state instruction)
    (set-reg state a (boole boole-and (+ rb rc) #xffffffff))))

;; multiplication
;; The register A receives the value in register B times the value in
;; register C, modulo 2^32.
(defun vmi-multiplication (state instruction)
  (debug-print state "mult")
  (multiple-value-bind (a b c ra rb rc) (abc-from-instruction state instruction)
    (set-reg state a (boole boole-and (* rb rc) #xffffffff))))

;; division
;; The register A receives the value in register B divided by the value in
;; register C, if any, where each quantity is treated as an unsigned 32 bit
;; number.
(defun vmi-division (state instruction)
  (debug-print state "divide")
  (multiple-value-bind (a b c ra rb rc) (abc-from-instruction state instruction)
    (set-reg state a (truncate (/ rb rc)))))

;; not-and
;; Each bit in the register A receives the 1 bit if either register B or
;; register C has a 0 bit in that position. Otherwise the bit in register A
;; receives the 0 bit.
(defun vmi-not-and (state instruction)
  (debug-print state "not-and")
  (multiple-value-bind (a b c ra rb rc) (abc-from-instruction state instruction)
    (set-reg state a (boole boole-and (boole boole-nand rb rc) #xffffffff))))

;; halt
;; The universal machine stops computation.
(defun vmi-halt (state instruction)
  (debug-print state "halt")
  (format t "~%(halt)~%")
  (setf (state-running state) nil)
  state)

;; allocation
;; A new array is created with a capacity of platters commensurate to the
;; value in the register C. This new array is initialized entirely with
;; platters holding the value 0. A bit pattern not consisting of exclusively
;; the 0 bit, and that identifies no other active allocated array, is placed
;; in the B register.
(defun vmi-allocation (state instruction)
  (debug-print state "alloc")
  (let ((i (length (state-arrays state))))
    (multiple-value-bind (a b c ra rb rc) (abc-from-instruction state instruction)
      (set-array state i (make-array rc :element-type 'integer :initial-element 0))
      (set-reg state b i))))

;; abandonment
;; The array identified by the register C is abandoned. Future allocations
;; may then reuse that identifier.
(defun vmi-abandonment (state instruction)
  (debug-print state "abandon")
  (multiple-value-bind (a b c ra rb rc) (abc-from-instruction state instruction)
    (set-array state rc nil)))

;; output
;; The value in the register C is displayed on the console immediately. Only
;; values between and including 0 and 255 are allowed.
(defun vmi-output (state instruction)
  ; (debug-print state "output")
  (multiple-value-bind (a b c ra rb rc) (abc-from-instruction state instruction)
    (format t "~c" (coerce rc 'character))
    (force-output)
    state))

;; input
;; The universal machine waits for input on the console. When input arrives,
;; the register C is loaded with the input, which must be between and
;; including 0 and 255. If the end of input has been signaled, then the
;; register C is endowed with a uniform value pattern where every place is
;; pregnant with the 1 bit.
(defun vmi-input (state instruction)
  (debug-print state "input")
  (multiple-value-bind (a b c ra rb rc) (abc-from-instruction state instruction)
    (let ((ch (read *standard-input*)))
      (set-reg state c (if (< 0 ch) -1 ch)))))

;; load program
;; The array identified by the B register is duplicated and the duplicate
;; shall replace the '0' array, regardless of size. The execution finger is
;; placed to indicate the platter of this array that is described by the
;; offset given in C, where the value 0 denotes the first platter, 1 the
;; second, et cetera.
;;
;; The '0' array shall be the most sublime choice for loading, and shall be
;; handled with the utmost velocity.
;;
;; Note: loading code array 0 is an idiom for jumping to the instruction
;; offset in c.
(defun vmi-load-program (state instruction)
  (debug-print state "loadprog")
  (multiple-value-bind (a b c ra rb rc) (abc-from-instruction state instruction)
    (let ((b-index rb))
      (unless (zerop b-index)           ; optimization: do nothing if nop
        (let ((src (nth-array state b-index)))
          (set-array state 0 (make-array (length src) :initial-contents src :element-type 'integer))))
      ; we set pc to one less since run-code increments the pc
      (setf (state-pc state) (1- rc))
      state)))

;; orthography
;; The value indicated is loaded into the register A forthwith.
(defun vmi-loadi (state instruction)
  (debug-print state "loadi")
  (set-reg state (loadi-a-reg instruction) (loadi-const instruction)))

(defvar *instructions*
  #(vmi-move                            ; 0
    vmi-array-index                     ; 1
    vmi-array-amendment                 ; 2
    vmi-addition                        ; 3
    vmi-multiplication                  ; 4
    vmi-division                        ; 5
    vmi-not-and                         ; 6
    vmi-halt                            ; 7
    vmi-allocation                      ; 8
    vmi-abandonment                     ; 9
    vmi-output                          ; 10
    vmi-input                           ; 11
    vmi-load-program                    ; 12
    vmi-loadi                           ; 13
    ))

(defun run-code (s)
  (loop for state = s
   while (state-running state)
   do (progn
        (let* ((instruction (instruction-at-pc state))
               (opcode (instruction-opcode instruction))
               (f (aref *instructions* opcode))
               (new-state (funcall f state instruction)))
          (incf (state-pc new-state))
          (setf state new-state)))))

(defun initial-state ()
  (make-state :pc 0
              :registers (make-array 8 :element-type 'integer :initial-element 0)
              :arrays (make-array 1 :adjustable t) ; will be filled with code
              :running t))

(defun run (file)
  (run-code (load-code (initial-state) file)))
