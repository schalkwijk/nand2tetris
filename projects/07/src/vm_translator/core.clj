(ns vm-translator.core
  (:require [clojure.string :as str] [clojure.java.io :as io]
            [vm-translator.push :refer :all]
            [vm-translator.pop :refer :all]
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

;; main
(defn translate [instructions]
  (let [cleaned-instructions (strip-out-comments-and-whitespace instructions)]
    (reduce #(merge-instructions %1 (parse-instruction %2 %1)) {:instruction-count 0 :instructions ""} cleaned-instructions)))

(defn -main [file]
  (with-open [rdr (io/reader file)]
    (with-open [wrtr (io/writer (str/replace file ".vm" ".asm"))]
      (.write wrtr (append-ancillary-functions (:instructions (translate (line-seq rdr))))))))
