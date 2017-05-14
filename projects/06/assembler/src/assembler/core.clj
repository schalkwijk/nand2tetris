(ns assembler.core
  (:require [clojure.string :as str]))

;; a instruction
(defn- decimal-to-binary [decimal]
  (str/replace (format "%15s" (Integer/toString decimal 2)) " " "0"))

(defn- parse-a-instruction [raw]
  {:instruction (str "0" (decimal-to-binary (Integer/parseInt (str/replace-first raw "@" ""))))})

;; c instruction
(defn- parse-c-cmp [cmp]
  (case cmp
    "A" "0110000"
    "D+A" "0000010"
    "D" "0001100"))

(defn- parse-c-dest [dest]
  (case dest
    "D" "010"
    "M" "001"))

(defn- parse-c-jump [jump]
  "000")

(defn- parse-c-instruction [raw]
  (let [[dest cmp] (str/split raw #"=")]
  {:instruction (str "111" (parse-c-cmp cmp) (parse-c-dest dest) (parse-c-jump "filler"))}))

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
