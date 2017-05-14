(ns assembler.core
  (:require [clojure.string :as str]))

(defn- decimal-to-binary [decimal]
  (str/replace (format "%15s" (Integer/toString decimal 2)) " " "0"))

(defn parse-a-instruction [& {:keys [raw]}]
  {:instruction (str "0" (decimal-to-binary (Integer/parseInt (str/replace-first raw "@" ""))))})

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

(defn parse-c-instruction [& {:keys [raw]}]
  (let [[dest cmp] (str/split raw #"=")]
  {:instruction (str "111" (parse-c-cmp cmp) (parse-c-dest dest) (parse-c-jump "filler"))}))

(defn -main [file]
  (with-open [rdr (clojure.java.io/reader file)]
    (print ((count (line-seq rdr))))))
