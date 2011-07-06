(ns
 #^{:author "Jim Menard"
    :doc "ICFP 2006 contest VM"}
 jimm.vm)

(defrecord State [pc registers arrays running])

(defn set-reg
  [state n val]
  (assoc state :registers (assoc (:registers state) n val)))

(defn ^long reg
  [state n]
  ((:registers state) n))

(defn set-array
  [state n val]
  (assoc state :arrays (assoc (:arrays state) n val)))

(defn array
  [state n]
  ((:arrays state) n))

(defn code
  [state]
  (array state 0))

; Load code from file and return new state containing code.
(defn load-code
  [state file]
  (with-open [r (java.io.FileInputStream. file)]
    (loop [code []
           c (.read r)]
      (if (neg? c)
        (set-array state 0 code) 
        (recur (conj code
                     (+ (bit-shift-left c         24)
                        (bit-shift-left (.read r) 16)
                        (bit-shift-left (.read r) 8)
                                        (.read r)))
               (.read r))))))

(defn instruction-at-pc
  [state]
  ((code state) (:pc state)))

(defn opcode-at-pc
  [state]
  (bit-and (bit-shift-right (instruction-at-pc state) 28) 0x0f))

(defn abc-from-instruction
  [state]
  (let [i (instruction-at-pc state)]
    [(bit-and (bit-shift-right i 6) 7)
     (bit-and (bit-shift-right i 3) 7)
     (bit-and i                    7)]))

(defn loadi-a-reg
  [instruction]
  (bit-and (bit-shift-right instruction 25) 7))

(defn loadi-const
  [instruction]
  (bit-and instruction 0x1ffffff))

; Set to *out* or some other output stream if you want debug output.
(def *debug-out* nil)

; Print debug message and return state. We return state so this can be the
; last call in a function that returns state.
(defn debug
  ([state msg]
   (debug state msg nil))
  ([state msg custom-out]
   (binding [*out* (or *debug-out* custom-out)]
     (when *out*
       (let [ins (instruction-at-pc state)
             opcode (bit-shift-right ins 28)
             [a b c] (abc-from-instruction state)]
         (if (= opcode 13)
           (printf "%08x    loadi %d, %d   " (:pc state) (loadi-a-reg ins) (loadi-const ins))
           (printf "%08x %8s %d, %d, %d", (:pc state) msg a b c))
         (println "\t; regs =" (:registers state))))
     state)))

(defmulti execute-instruction opcode-at-pc)

; move
; The register A receives the value in register B,
; unless the register C contains 0.
(defmethod execute-instruction 0
  [state]
  (debug state "move")
  (let [[^long a ^long b ^long c] (abc-from-instruction state)]
    (if (zero? (reg state c))
      state
      (set-reg state a (reg state b)))))

; array index
; The register A receives the value stored at offset
; in register C in the array identified by B.
(defmethod execute-instruction 1
  [state]
  (debug state "aindex")
  (let [[^long a ^long b ^long c] (abc-from-instruction state)]
    (set-reg state a ((array state (reg state b)) (reg state c)))))

; array amendment
; The array identified by A is amended at the offset
; in register B to store the value in register C.
(defmethod execute-instruction 2
  [state]
  (debug state "aamend")
  (let [[^long a ^long b ^long c] (abc-from-instruction state)
        a-index (reg state a)]
    (set-array state a-index
               (assoc (array state a-index) (reg state b) (reg state c)))))

; addition
; The register A receives the value in register B plus 
; the value in register C, modulo 2^32.
(defmethod execute-instruction 3
  [state]
  (debug state "add")
  (let [[^long a ^long b ^long c] (abc-from-instruction state)]
    (set-reg state a (+ (reg state b) (reg state c)))))

; multiplication
; The register A receives the value in register B times
; the value in register C, modulo 2^32.
(defmethod execute-instruction 4
  [state]
  (debug state "mult")
  (let [[^long a ^long b ^long c] (abc-from-instruction state)]
    (set-reg state a (bit-and (* (reg state b) (reg state c)) 0xffffffff))))

; division
; The register A receives the value in register B
; divided by the value in register C, if any, where
; each quantity is treated treated as an unsigned 32
; bit number.
(defmethod execute-instruction 5
  [state]
  (debug state "divide")
  (let [[^long a ^long b ^long c] (abc-from-instruction state)]
    ; Need to fool with casting to force Java division, not Clojure division
    ; that result in a ratio.
    (set-reg state a (long (/ (long (bit-and (reg state b) 0xffffffff)) 
                             (long (bit-and (reg state c) 0xffffffff)))))))

; not-and
; Each bit in the register A receives the 1 bit if
; either register B or register C has a 0 bit in that
; position.  Otherwise the bit in register A receives
; the 0 bit.
(defmethod execute-instruction 6
  [state]
  (debug state "not-and")
  (let [[^long a ^long b ^long c] (abc-from-instruction state)]
    ;; (set-reg state a (bit-and (bit-not (bit-and (reg state b) (reg state c))) 0xffffffff))))
    (set-reg state a (bit-not (bit-and (reg state b) (reg state c))))))

; halt
; The universal machine stops computation.
(defmethod execute-instruction 7
  [state]
  (debug state "halt")
  (println "\n(halt)")
  (assoc state :running false))

; allocation
; A new array is created with a capacity of platters
; commensurate to the value in the register C. This
; new array is initialized entirely with platters
; holding the value 0. A bit pattern not consisting of
; exclusively the 0 bit, and that identifies no other
; active allocated array, is placed in the B register.
(defmethod execute-instruction 8
  [state]
  (debug state "alloc")
  (let [[^long a ^long b ^long c] (abc-from-instruction state)
        i (count (:arrays state))
        zeroed-data (into [] (take (reg state c) (repeat 0)))]
    (set-reg
     (set-array state i zeroed-data)
     b i)))

; abandonment
; The array identified by the register C is abandoned.
; Future allocations may then reuse that identifier.
(defmethod execute-instruction 9
  [state]
  (debug state "abandon")
  (let [[^long a ^long b ^long c] (abc-from-instruction state)]
    (set-array state (reg state c) ())))

; output
; The value in the register C is displayed on the console
; immediately. Only values between and including 0 and 255
; are allowed.
(defmethod execute-instruction 10
  [state]
  ; (debug state "output")
  (let [[^long a ^long b ^long c] (abc-from-instruction state)]
    (print (format "%c" (char (reg state c))))
    (flush)
    state))

; input
; The universal machine waits for input on the console.
; When input arrives, the register C is loaded with the
; input, which must be between and including 0 and 255.
; If the end of input has been signaled, then the 
; register C is endowed with a uniform value pattern
; where every place is pregnant with the 1 bit.
(defmethod execute-instruction 11
  [state]
  (debug state "input")
  (let [[^long a ^long b ^long c] (abc-from-instruction state)
        ch (.read *in*)]
    (set-reg state c (if (neg? ch) -1 ch))))

; load program
; The array identified by the B register is duplicated
; and the duplicate shall replace the '0' array,
; regardless of size. The execution finger is placed
; to indicate the platter of this array that is
; described by the offset given in C, where the value
; 0 denotes the first platter, 1 the second, et
; cetera.
;
; The '0' array shall be the most sublime choice for
; loading, and shall be handled with the utmost
; velocity.
;
; Note: loading code array 0 is an idiom for jumping
; to the instruction offset in c.
(defmethod execute-instruction 12
  [state]
  (debug state "loadprog")
  (let [[^long a ^long b ^long c] (abc-from-instruction state)
        b-index (reg state b)
        state1 (if (zero? b-index)      ; optimization
                 state                  ; do nothing if nop
                 (set-array state 0 (array state b-index)))]
    ; we set pc to one less since run-code increments the pc
    (assoc state1 :pc (dec (reg state1 c)))))

; orthography
; The value indicated is loaded into the register A
; forthwith.
(defmethod execute-instruction 13
  [state]
  (debug state "loadi")
  (let [ins (instruction-at-pc state)]
    (set-reg state (loadi-a-reg ins) (loadi-const ins))))

(defn run-code
  [initial-state]
  (loop [state initial-state]
    (when (:running state)
      (recur (execute-instruction (assoc state :pc (inc (:pc state))))))))

(defn run
  [file]
  (let [state (State. 0 [0 0 0 0 0 0 0 0] [] true)]
      (run-code (load-code state file))))
