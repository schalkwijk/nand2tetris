(ns assembler.symbol-table
  (:require [clojure.string :as str]))

(def default-symbol-table
  (hash-map
   "SCREEN" "0000000000000000"
   "KDB" "0100000000000000"
   "SP" "0000000000000000"
   "ARG" "0000000000000010"
   "LCL" "0000000000000001"
   "THIS" "0000000000000011"
   "THAT" "0000000000000100"))

(defn- decimal-to-binary [decimal]
  (str/replace (format "%16s" (Integer/toString decimal 2)) " " "0"))

(defn- generate-symbol-table-from-instructions-recur [instructions instruction-count table]
  (if (= 0 (count instructions)) table
      (let [label (second (re-matches #"^\((.*)\)$" (first instructions)))]
        (if (nil? label)
          (recur (rest instructions) (+ 1 instruction-count) table)
          (recur (rest instructions) instruction-count (assoc table label (decimal-to-binary instruction-count)))))))

(defn- generate-symbol-table-from-instructions [instructions]
  (generate-symbol-table-from-instructions-recur instructions 0 {}))

(defn generate-symbol-table [instructions]
  (merge default-symbol-table (generate-symbol-table-from-instructions instructions)))
