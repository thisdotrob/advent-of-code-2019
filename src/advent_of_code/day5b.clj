(ns advent-of-code.day5b)

;; Opcode = position 1

;; Opcodes:
;;    99 terminate
;;    1  add next 2 params, store in position given by third
;;    2  multiply first 2 params, store in position given by third
;;    3  store input value at position given by 1st (& only) param
;;    4  output the value at position given by 1st (& only) param

;; After running instruction, move pointer to position after its last param

;; Param modes:
;;    0  position mode: params are interpreted as positions of the value
;;    1  immediate mode: params are interpreted as the value

;; Parsing opcode & param mode:
;;    last 2 digits = opcode e.g. 02 -> 2
;;    1 digit to left of this = mode of 1st param
;;    2 digits to left of this = mode of 2nd param
;;    3 digits to left of this = mode of 3rd param

;; Implentation design 1:
;; Move pointer forward by length of previous instruction
;; Extract the next instruction:
;;     parse the opcode
;;     calculate number of elements to take for the instruction based on this
;; Substitute values if in position mode:
;;     requires access to full program
;; Evaluate instruction calculation
;; Store result in program (if not opcode 4)
;; Recur, providing pointer position, program and instruction just completed
;; If pointer position is nil, default to 0
;; If prev instruction is nil, move forward

(defn parse-int [str]
  (Integer/parseInt str))

(def pointer-val "1002")
(def split (split-at (- (count pointer-val) 2) pointer-val))
(def opcode (-> split second clojure.string/join parse-int))
opcode

(defn next-instruction [program pointer]
  (let [pointer-val (nth program pointer)
        [param-modes opcode] (split-at (- (count pointer-val) 2) pointer-val)
        opcode ()]))

(defn step [program pointer prev-instruction]
  (let [next-pointer (+ pointer (:length prev-instruction))
        instruction (next-instruction program next-pointer)]))
