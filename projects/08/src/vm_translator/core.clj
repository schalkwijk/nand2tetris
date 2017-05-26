(ns vm-translator.core
  (:require [clojure.string :as str] [clojure.java.io :as io]
            [vm-translator.push :refer :all]
            [vm-translator.pop :refer :all]
            [vm-translator.function :refer :all]
            [vm-translator.ancillary :refer :all])
  (:gen-class))

(defn- instruction-is-not-comment [instruction]
  (not (-> instruction
           (str/split #"//")
           first
           str/trim
           str/blank?)))

(defn- strip-out-comments-and-whitespace [instructions]
  (map #(str/trim (first (str/split % #"//"))) (filter instruction-is-not-comment instructions)))

(defn- generate-two-arg-command [operand]
  ["@SP" "A=M-1" "D=M" "A=A-1" "A=M" (str "D=A" operand "D") "@SP" "A=M" "A=A-1" "A=A-1" "M=D" "D=A+1" "@SP" "M=D"])

(defn- generate-one-arg-command [operand]
  ["@SP" "A=M-1" "D=M" (str "D=" operand "D") "M=D"])

(defn- generate-comp-command [label merged-instruction-metadata]
  [(str "@" (+ (:instruction-count merged-instruction-metadata) 6)) "D=A" "@R13" "M=D" (str "@" label) "0;JMP"])

(defn- generate-label-command [label]
  [(str "(" label ")")])

(defn- generate-if-goto-command [label]
  ["@SP" "M=M-1" "A=M" "D=M" (str "@" label) "D;JNE"])

(defn- generate-goto-command [label]
  [(str "@" label) "0;JMP"])

;; helpers
(defn- add-instruction-metadata [asm-instructions]
  {:instructions asm-instructions :instruction-count (count asm-instructions)})

(defn- merge-instructions [merged-instruction-metadata current-instruction-metadata]
  (assoc merged-instruction-metadata
         :instruction-count (+ (:instruction-count merged-instruction-metadata) (:instruction-count current-instruction-metadata))
         :instructions (concat (:instructions merged-instruction-metadata) (:instructions current-instruction-metadata))))

(defn- parse-instruction [instruction merged-instruction-metadata]
  (let [split-args (str/split instruction #" ")]
    (add-instruction-metadata
     (case (first split-args)
       "pop" (apply generate-pop-command (concat (rest split-args) [(:filename merged-instruction-metadata)]))
       "push" (apply generate-push-command (concat (rest split-args) [(:filename merged-instruction-metadata)]))
       "add" (generate-two-arg-command "+")
       "sub" (generate-two-arg-command "-")
       "and" (generate-two-arg-command "&")
       "or" (generate-two-arg-command "|")
       "neg" (generate-one-arg-command "-")
       "not" (generate-one-arg-command "!")
       "label" (generate-label-command (last split-args))
       "if-goto" (generate-if-goto-command (last split-args))
       "goto" (generate-goto-command (last split-args))
       "function" (apply generate-function-command (rest split-args))
       "return" (generate-return-command)
       "eq" (generate-comp-command "EQ_OP" merged-instruction-metadata)
       "gt" (generate-comp-command "GT_OP" merged-instruction-metadata)
       "lt" (generate-comp-command "LT_OP" merged-instruction-metadata)))))

(defn translate [instructions filename]
  (let [cleaned-instructions (strip-out-comments-and-whitespace instructions)]
    (:instructions (reduce
      #(merge-instructions %1 (parse-instruction %2 %1))
      {:filename filename :instruction-count 0 :instructions ""}
      cleaned-instructions))))

;; main
(defn- translate-instructions-into-file [wrtr rdr filename-without-extension]
  (.write wrtr (str/join "\n" (translate (line-seq rdr) filename-without-extension))))

(defn -main [filename]
  (let [file (io/file filename)
        filename-without-extension (str/replace (.getName file) ".vm" "")
        output-filename (str/replace filename ".vm" ".asm")]
    (with-open [wrtr (io/writer output-filename)]
      (with-open [rdr (io/reader file)]
        (translate-instructions-into-file wrtr rdr filename-without-extension)
        (.write wrtr (str/join "\n" (conj ancillary-functions "\n")))))))
