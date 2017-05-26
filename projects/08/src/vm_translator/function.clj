(ns vm-translator.function
  (:require [clojure.string :as str]))

(defn generate-function-command [function-name number-of-local-args filename]
  (concat [(str "(" filename "." function-name ")")] (apply concat (repeat (Integer. number-of-local-args) ["@SP" "A=M" "M=0" "@SP" "M=M+1"]))))

(defn- restore-memory-segment [segment offset]
  [(str "@" offset) "D=A" "@R14" "A=M-D" "D=M" (str "@" segment) "M=D"])

(defn generate-return-command []
  (concat ["@LCL" "D=M" "@5" "A=D-A" "D=M" "@R13" "M=D" ;; set retAddr to R13
           "@LCL" "D=M" "@R14" "M=D"] ;; set frameEnd to R14
          ["@SP" "M=M-1" "A=M" "D=M" "@ARG" "A=M" "M=D" "@ARG" "D=M+1" "@SP" "M=D"]
          (restore-memory-segment "LCL" "4")
          (restore-memory-segment "ARG" "3")
          (restore-memory-segment "THIS" "2")
          (restore-memory-segment "THAT" "1")
          ["@5" "D=A" "@R14" "A=M-D" "A=M" "0;JMP"]))
