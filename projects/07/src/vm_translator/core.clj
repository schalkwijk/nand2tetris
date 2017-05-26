(ns vm-translator.core
  (:require [clojure.string :as str] [clojure.java.io :as io])
  (:gen-class))

(defn- instruction-is-not-comment [instruction]
  (not (-> instruction
           (str/split #"//")
           first
           str/trim
           str/blank?)))

(defn- strip-out-comments-and-whitespace [instructions]
  (map #(str/trim (first (str/split % #"//"))) (filter instruction-is-not-comment instructions)))

;; push commands
(defn- generate-constant-push-command [value]
  [(str "@" value ) "D=A" "@SP" "A=M" "M=D" "@SP" "M=M+1"])

(defn- generate-relative-push-command [base-address location]
  [(str "@" base-address ) "D=M" (str "@" location) "D=D+A" "A=D" "D=M" "@SP" "A=M" "M=D" "D=A+1" "@SP" "M=D"])

(defn- generate-temp-push-command [location]
  ["@R5" "D=A" (str "@" location) "D=D+A" "A=D" "D=M" "@SP" "A=M" "M=D" "D=A+1" "@SP" "M=D"])

(defn- generate-static-push-command [location]
  [(str "@Foo." location) "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"])

(defn- generate-push-command [memory-segment location]
  (case memory-segment
    "constant" (generate-constant-push-command location)
    "local" (generate-relative-push-command "LCL" location)
    "argument" (generate-relative-push-command "ARG" location)
    "this" (generate-relative-push-command "THIS" location)
    "that" (generate-relative-push-command "THAT" location)
    "static" (generate-static-push-command location)
    "temp" (generate-temp-push-command location)))

;; pop commands
(defn- generate-relative-pop-command [base-address location]
  ["@SP" "M=M-1" (str "@" base-address) "D=M" (str "@" location) "D=D+A" "@R13" "M=D" "@SP" "A=M" "D=M" "@R13" "A=M" "M=D"])

(defn- generate-temp-pop-command [location]
  ["@SP" "M=M-1" "@R5" "D=A" (str "@" location) "D=D+A" "@R13" "M=D" "@SP" "A=M" "D=M" "@R13" "A=M" "M=D"])

(defn- generate-static-pop-command [location]
  ["@SP" "A=M-1" "D=M" (str "@Foo." location) "M=D" "@SP" "M=M-1"])

(defn- generate-pop-command [memory-segment location]
  (case memory-segment
    "local" (generate-relative-pop-command "LCL" location)
    "argument" (generate-relative-pop-command "ARG" location)
    "this" (generate-relative-pop-command "THIS" location)
    "that" (generate-relative-pop-command "THAT" location)
    "static" (generate-static-pop-command location)
    "temp" (generate-temp-pop-command location)))

(defn- generate-two-arg-command [operand]
  ["@SP" "A=M-1" "D=M" "A=A-1" "A=M" (str "D=A" operand "D") "@SP" "A=M" "A=A-1" "A=A-1" "M=D" "D=A+1" "@SP" "M=D"])

(defn- generate-one-arg-command [operand]
  ["@SP" "A=M-1" "D=M" (str "D=" operand "D") "M=D"])

;; eq command
(defn- generate-comp-command [label merged-instruction-metadata]
  [(str "@" (+ (:instruction-count merged-instruction-metadata) 6)) "D=A" "@R13" "M=D" (str "@" label) "0;JMP"])

;; helpers
(defn- decorate-instructions [asm-instructions]
  {:instructions (str/join "\n" asm-instructions)
   :instruction-count (count asm-instructions)})

(defn- merge-instructions [merged-instruction-metadata current-instruction-metadata]
  (assoc merged-instruction-metadata
         :instruction-count (+ (:instruction-count merged-instruction-metadata) (:instruction-count current-instruction-metadata))
         :instructions (str (:instructions merged-instruction-metadata) "\n" (:instructions current-instruction-metadata))))

(defn- parse-instruction [instruction merged-instruction-metadata]
  (let [split-args (str/split instruction #" ")]
    (decorate-instructions
     (case (first split-args)
       "pop" (apply generate-pop-command (rest split-args))
       "push" (apply generate-push-command (rest split-args))
       "add" (generate-two-arg-command "+")
       "sub" (generate-two-arg-command "-")
       "and" (generate-two-arg-command "&")
       "or" (generate-two-arg-command "|")
       "neg" (generate-one-arg-command "-")
       "not" (generate-one-arg-command "!")
       "eq" (generate-comp-command "EQ_OP" merged-instruction-metadata)
       "gt" (generate-comp-command "GT_OP" merged-instruction-metadata)
       "lt" (generate-comp-command "LT_OP" merged-instruction-metadata)))))

;; ancillary functions
(defn- construct-comp-function [label jump-condition]
  (str/join "\n" [(str "(" label ")") "@SP" "A=M-1" "D=M" "A=A-1" "A=M" "D=A-D" "@OP_FINISHED_TRUE" (str "D;" jump-condition) "D=0" "@OP_FINISHED_FALSE" "0;JMP"]))

(def terminal-loop
  (str/join "\n" ["// terminal loop" "(END)" "@END" "0;JMP"]))

(def op-success
  (str/join "\n" ["(OP_FINISHED_TRUE)" "D=-1" "(OP_FINISHED_FALSE)" "@SP" "A=M" "A=A-1" "A=A-1" "M=D" "D=A+1" "@SP" "M=D" "@R13" "A=M" "0;JMP"]))

(def eq-function
  (construct-comp-function "EQ_OP" "JEQ"))

(def lt-function
  (construct-comp-function "LT_OP" "JLT"))

(def gt-function
  (construct-comp-function "GT_OP" "JGT"))

(defn- append-ancillary-functions [instructions]
  (str/join "\n" [instructions "// ancillary" terminal-loop eq-function lt-function gt-function op-success]))

;; main
(defn translate [instructions]
  (let [cleaned-instructions (strip-out-comments-and-whitespace instructions)]
    (reduce #(merge-instructions %1 (parse-instruction %2 %1)) {:instruction-count 0 :instructions ""} cleaned-instructions)))

(defn -main [file]
  (with-open [rdr (io/reader file)]
    (with-open [wrtr (io/writer (str/replace file ".vm" ".asm"))]
      (.write wrtr (append-ancillary-functions (:instructions (translate (line-seq rdr))))))))

;; todo
;; neg
;; not
