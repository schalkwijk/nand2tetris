(ns vm-translator.core
  (:require [clojure.string :as str]))

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

(defn- generate-push-command [memory-segment location]
  (case memory-segment
    "constant" (generate-constant-push-command location)
    ;; (generate-relative-push-command memory-segment location)
    ))

;; pop commands
(defn- generate-relative-pop-command [pointer location]
  (str/join "\n" ["@SP" "M=M-1" (str "@" pointer) "D=M" (str "@" location) "D=D+A" "@R13" "M=D" "@SP" "A=M" "D=M" "@R13" "A=M" "M=D"]))

(defn- generate-pop-command [memory-segment location]
  (case memory-segment
    "local" (generate-relative-pop-command "LCL" location)
    "argument" (generate-relative-pop-command "ARG" location)
    "this" (generate-relative-pop-command "THIS" location)
    "that" (generate-relative-pop-command "THAT" location)))

(defn- generate-sub-command [])
(defn- generate-add-command [])

(defn- parse-instruction [instruction]
  (let [split-args (str/split instruction #" ")]
        (case (first split-args)
          "pop" (apply generate-pop-command (rest split-args))
          "push" (apply generate-push-command (rest split-args))
          "add" (generate-add-command)
          "sub" (generate-sub-command))))

;; main
(defn translate [instructions]
  (let [cleaned-instructions (strip-out-comments-and-whitespace instructions)]
    (map #(parse-instruction %) cleaned-instructions)))

(defn -main [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (dorun (map println (translate (line-seq rdr))))))
