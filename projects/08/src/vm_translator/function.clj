(ns vm-translator.function
  (:require [clojure.string :as str]))

;; function command
(defn generate-function-command [function-name number-of-local-args]
  (concat [(str "(" function-name ")")] (apply concat (repeat (Integer. number-of-local-args) ["@SP" "A=M" "M=0" "@SP" "M=M+1"]))))

;; return command
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

;; call command
(def increment-stack-pointer ["@SP" "M=M+1"])

(defn assign-segment-to-stack-pointer [segment]
  [(str "@" segment) "D=M" "@SP" "A=M" "M=D"])

(defn assign-and-increment-stack-pointer [segment]
  (concat (assign-segment-to-stack-pointer segment) increment-stack-pointer))

(defn- store-current-frame [number-of-args function-name]
  (concat
   (assign-and-increment-stack-pointer "LCL")
   (assign-and-increment-stack-pointer "ARG")
   (assign-and-increment-stack-pointer "THIS")
   (assign-and-increment-stack-pointer "THAT")
   ["@SP" "D=M" "@LCL" "M=D"]
   ["@LCL" "D=M" "@5" "D=D-A" (str "@" number-of-args) "D=D-A" "@ARG" "M=D"]))

(defn generate-call-command [function-name number-of-args instruction-count]
  (concat
   [(str "@function-reentry-point." instruction-count) "D=A" "@SP" "A=M" "M=D"]
   increment-stack-pointer
   (store-current-frame number-of-args function-name)
   [(str "@" function-name) "0;JMP" (str "(function-reentry-point." instruction-count ")")]))
