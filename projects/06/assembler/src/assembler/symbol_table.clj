(ns assembler.symbol-table
  (:require [clojure.string :as str]))

(use 'flatland.ordered.map)

(defn- decimal-to-binary [decimal]
  (str/replace (format "%16s" (Integer/toString decimal 2)) " " "0"))

(def default-symbol-table
  (merge (ordered-map
          "@SCREEN" "0100000000000000"
          "@KDB" "0100000000000000"
          "@SP" "0000000000000000"
          "@ARG" "0000000000000010"
          "@LCL" "0000000000000001"
          "@THIS" "0000000000000011"
          "@THAT" "0000000000000100")
         (reduce #(assoc %1 (str "@R" %2) (decimal-to-binary %2)) {} (take 16 (range)))))

(defn- generate-symbol-table-from-instructions-recur [instructions instruction-count table]
  (if (= 0 (count instructions)) table
      (let [label (second (re-matches #"^\((.*)\)$" (first instructions)))
            variable (re-matches #"@[^0-9].*$" (first instructions))]
        (cond
          (not (nil? label))
          (recur (rest instructions) instruction-count (assoc table (str "@" label) (decimal-to-binary instruction-count)))

          (and (not (nil? variable)) (nil? (get table variable)))
          (recur (rest instructions) (+ 1 instruction-count) (assoc table variable nil))

          :else (recur (rest instructions) (+ 1 instruction-count) table)))))

(defn- generate-symbol-table-from-instructions [instructions]
  (generate-symbol-table-from-instructions-recur instructions 0 default-symbol-table))

(defn- assign-addresses-to-variables-recur [nil-keys current-count symbol-table]
  (if (= 0 (count nil-keys)) symbol-table
    (recur (rest nil-keys) (+ 1 current-count) (assoc symbol-table (first nil-keys) (decimal-to-binary (+ 16 current-count))))))

(defn- assign-addresses-to-variables [symbol-table]
  (assign-addresses-to-variables-recur (map key (filter #(nil? (val %)) symbol-table)) 0 symbol-table))

(defn generate-symbol-table [instructions]
  (assign-addresses-to-variables (generate-symbol-table-from-instructions (map str/trim instructions))))
