(ns assembler.core
  (:require [clojure.string :as str]))

;; a instruction
(def symbol-table
  (hash-map
   "SCREEN" "0000000000000000"
   "KDB" "0100000000000000"
   "SP" "0000000000000000"
   "ARG" "0000000000000010"
   "LCL" "0000000000000001"
   "THIS" "0000000000000011"
   "THAT" "0000000000000100"))

(defn- decimal-to-binary [decimal]
  (str/replace (format "%15s" (Integer/toString decimal 2)) " " "0"))

(defn- parse-a-instruction [raw]
  (let [symbolOrNumber (str/replace-first raw "@" "")]
    (if (contains? symbol-table symbolOrNumber)
      {:instruction (get symbol-table symbolOrNumber)}
      {:instruction (str "0" (decimal-to-binary (Integer/parseInt symbolOrNumber)))})))

;; c instruction
(def c-instruction-regex #"^(?:([MDA]{1,3})=)?([-+!]?(?:A|M|D|1|1|0)(?:[+-\|\&][AMD10])?)(?:;(J[GELNM][QTEP]))?$")

(def cmp-instruction-map
  (hash-map
   #"[AM]" "110000"
   #"![AM]" "110001"
   #"D\+[AM]" "000010"
   #"D" "001100"
   #"0" "101010"
   #"1" "111111"
   #"-1" "111010"
   #"D\+1" "011111"
   #"[AM]\+1" "110111"
   #"[AM]-1" "110010"
   #"D-1" "001110"
   #"D-[AM]" "010011"
   #"-D" "001111"
   #"[AM]-D" "000111"
   #"-[AM]" "110011"
   #"D&[AM]" "000000"
   #"D\|[AM]" "010101"
   ))

(defn- parse-c-cmp [cmp]
  (let [a (if (str/includes? cmp "M") "1" "0")]
    (str a (nth (first (filter #(re-matches (first %1) cmp) (seq cmp-instruction-map))) 1))))

(defn- parse-c-dest [dest]
  (case dest
    "M" "001"
    "D" "010"
    "MD" "011"
    "A" "100"
    "AM" "101"
    "AD" "110"
    "AMD" "111"
    "000"))

(defn- parse-c-jump [jump]
  (case jump
    "JMP" "111"
    "JGT" "001"
    "JEQ" "010"
    "JGE" "011"
    "JLT" "100"
    "JNE" "101"
    "JLE" "110"
    "000"))

(defn- parse-c-instruction [raw]
  (let [[match dst cmp jmp] (re-matches c-instruction-regex raw)]
  {:instruction (str "111" (parse-c-cmp cmp) (parse-c-dest dst) (parse-c-jump jmp))}))

;; whitespace handling
(defn- strip-out-whitespace-and-comments [instruction]
  (-> instruction
      (str/split #"//")
      first
      str/trim))

(defn- strip-out-whitespace-and-comments-from-instructions [instructions]
  (filter #(not (str/blank? %)) (map (fn [instr] (strip-out-whitespace-and-comments instr)) instructions)))

;; main
(defn parse-instruction [& {:keys [raw]}]
  (if (= (subs raw 0 1) "@")
    (parse-a-instruction raw)
    (parse-c-instruction raw)))

(defn assemble [instructions]
  (map #(:instruction (parse-instruction :raw %)) (strip-out-whitespace-and-comments-from-instructions instructions)))

(defn -main [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (dorun (map println (assemble (line-seq rdr))))))
