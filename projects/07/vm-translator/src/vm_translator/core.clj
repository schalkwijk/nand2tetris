(ns vm-translator.core
  (:require [clojure.string :as str])
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
  (str/join "\n" [(str "@" value ) "D=A" "@SP" "A=M" "M=D" "@SP" "M=M+1"]))

(defn- generate-relative-push-command [base-address location]
  (str/join "\n" [(str "@" base-address ) "D=M" (str "@" location) "D=D+A" "A=D" "D=M" "@SP" "A=M" "M=D" "D=A+1" "@SP" "M=D"]))

(defn- generate-temp-push-command [location]
  (str/join "\n" ["@R5" "D=A" (str "@" location) "D=D+A" "A=D" "D=M" "@SP" "A=M" "M=D" "D=A+1" "@SP" "M=D"]))

(defn- generate-push-command [memory-segment location]
  (case memory-segment
    "constant" (generate-constant-push-command location)
    "local" (generate-relative-push-command "LCL" location)
    "argument" (generate-relative-push-command "ARG" location)
    "this" (generate-relative-push-command "THIS" location)
    "that" (generate-relative-push-command "THAT" location)
    "temp" (generate-temp-push-command location)))

;; pop commands
(defn- generate-relative-pop-command [base-address location]
  (str/join "\n" ["@SP" "M=M-1" (str "@" base-address) "D=M" (str "@" location) "D=D+A" "@R13" "M=D" "@SP" "A=M" "D=M" "@R13" "A=M" "M=D"]))

(defn- generate-temp-pop-command [location]
  (str/join "\n" ["@SP" "M=M-1" "@R5" "D=A" (str "@" location) "D=D+A" "@R13" "M=D" "@SP" "A=M" "D=M" "@R13" "A=M" "M=D"]))

(defn- generate-pop-command [memory-segment location]
  (case memory-segment
    "local" (generate-relative-pop-command "LCL" location)
    "argument" (generate-relative-pop-command "ARG" location)
    "this" (generate-relative-pop-command "THIS" location)
    "that" (generate-relative-pop-command "THAT" location)
    "temp" (generate-temp-pop-command location)))

(defn- generate-add-sub-command [operand]
  (str/join "\n" ["@SP" "A=M-1" "D=M" "A=A-1" "A=M" (str "D=A" operand "D") "@SP" "A=M" "A=A-1" "A=A-1" "M=D" "D=A+1" "@SP" "M=D"]))

(defn- parse-instruction [instruction]
  (let [split-args (str/split instruction #" ")]
        (case (first split-args)
          "pop" (apply generate-pop-command (rest split-args))
          "push" (apply generate-push-command (rest split-args))
          "add" (generate-add-sub-command "+")
          "sub" (generate-add-sub-command "-"))))

;; main
(defn translate [instructions]
  (let [cleaned-instructions (strip-out-comments-and-whitespace instructions)]
    (map #(parse-instruction %) cleaned-instructions)))

(defn -main [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (dorun (map println (translate (line-seq rdr))))))


;; todo
;; support add
;; support sub
