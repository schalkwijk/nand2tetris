(ns vm-translator.ancillary
  (:require [clojure.string :as str]))

(defn- construct-comp-function [label jump-condition]
  [(str "(" label ")") "@SP" "A=M-1" "D=M" "A=A-1" "A=M" "D=A-D" "@OP_FINISHED_TRUE" (str "D;" jump-condition) "D=0" "@OP_FINISHED_FALSE" "0;JMP"])

(def terminal-loop
  ["// terminal loop" "(END)" "@END" "0;JMP"])

(def op-success
  ["(OP_FINISHED_TRUE)" "D=-1" "(OP_FINISHED_FALSE)" "@SP" "A=M" "A=A-1" "A=A-1" "M=D" "D=A+1" "@SP" "M=D" "@R13" "A=M" "0;JMP"])

(def eq-function
  (construct-comp-function "EQ_OP" "JEQ"))

(def lt-function
  (construct-comp-function "LT_OP" "JLT"))

(def gt-function
  (construct-comp-function "GT_OP" "JGT"))

(def ancillary-functions
  (concat terminal-loop eq-function lt-function gt-function op-success))
