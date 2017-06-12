(ns compiler.symbol-table
  (:require [clojure.zip :as zip]
            [compiler.zip-helpers :refer :all]))

(def var-keyword-to-scope
  {"var" :local})

(defn get-symbol-by-name [symbol-name table]
  (first (filter #(= (:name %) symbol-name) table)))

(defn get-scope-variable-count [scope table]
  (count (filter #(= (:scope %) scope) table)))

(defn- add-argument-to-table [table zipper count]
  (if (nil? zipper)
    table
    (let [zipper (if (= (zip/node (zip/down zipper)) ",") (zip/right zipper) zipper)
          {argument-type :value zipper :zipper} (fetch-node-content zipper)
          {argument-name :value zipper :zipper} (zip-and-fetch-node-content zipper [zip/right])]
      (recur (conj table {:name argument-name :type argument-type :position count :scope :argument})
             (zip/right zipper) (inc count)))))

(defn- add-local-var-to-table [var-keyword var-type var-name table]
  (let [current-var-count (get-scope-variable-count :local table)]
    (conj table {:name var-name :type var-type :position current-var-count :scope (get var-keyword-to-scope var-keyword)})))

(defn- add-vars-to-table [original-table-size table zipper counter]
  (let [var-keyword (zip/node (zip/down zipper))
        type (zip/node (zip/down (zip/right zipper)))
        vars-and-symbols (zip/rights (zip/right zipper))]
    (reduce #(add-local-var-to-table var-keyword type (first (:content %2)) %1) table
            (filter #(not (= (:tag %) :symbol)) vars-and-symbols))))

(defn- recur-add-local-vars-to-table [table zipper counter original-table-size]
  (if (not (= :varDec (:tag (zip/node zipper))))
    {:symbol-table table :zipper zipper}
    (let [var-zipper (zip/down (zip/xml-zip (zip/node zipper)))
          table (add-vars-to-table original-table-size table var-zipper counter)]
      (recur table (zip/right zipper) (count (filter #(= (:scope %) :local) table)) original-table-size))))

(defn create-table-for-expression-list [zipper]
  (add-argument-to-table [] (zip/down zipper) 0))

(defn add-local-vars-to-table [zipper table]
  (recur-add-local-vars-to-table table zipper 0 (count table)))
