(ns compiler.symbol-table
  (:require [clojure.zip :as zip]
            [compiler.zip-helpers :refer :all]))

(def var-keyword-to-scope
  {"var" :local
   "field" :this
   "static" :static})

(defn get-symbol-by-name [symbol-name table]
  (first (filter #(= (:name %) symbol-name) table)))

(defn get-scope-variable-count [scope table]
  (count (filter #(= (:scope %) scope) table)))

(defn- add-argument-to-table [table zipper count]
  (if (nil? zipper)
    table
    (let [zipper (if (= (fetch-node-content zipper) ",") (zip/right zipper) zipper)
          argument-type (fetch-node-content zipper)
          {argument-name :value zipper :zipper} (zip-and-fetch-node-content zipper [zip/right])]
      (recur (conj table {:name argument-name :type argument-type :position count :scope :argument})
             (zip/right zipper) (inc count)))))

(defn- add-local-var-to-table [var-keyword var-type var-name table]
  (let [current-var-scope (get var-keyword-to-scope var-keyword)
        current-var-count (get-scope-variable-count current-var-scope table)]
    (conj table {:name var-name :type var-type :position current-var-count :scope current-var-scope})))

(defn- add-vars-to-table [original-table-size table zipper]
  (let [var-keyword (fetch-node-content zipper)
        {type :value zipper :zipper} (zip-and-fetch-node-content zipper [zip/right])
        vars-and-symbols (zip/rights zipper)]
    (reduce #(add-local-var-to-table var-keyword type (first (:content %2)) %1) table
            (filter #(not (= (:tag %) :symbol)) vars-and-symbols))))

(defn- recur-add-local-vars-to-table [table zipper original-table-size]
  (if (or (nil? zipper) (not (or (= :classVarDec (:tag (zip/node zipper)))
                                 (= :varDec (:tag (zip/node zipper))))))
    {:symbol-table table :zipper zipper}
    (let [var-zipper (zip/down (zip/xml-zip (zip/node zipper)))
          table (add-vars-to-table original-table-size table var-zipper)]
      (recur table (zip/right zipper) original-table-size))))

(defn create-table-for-expression-list [zipper table]
  (add-argument-to-table table (zip/down zipper) 0))

(defn add-local-vars-to-table [zipper table]
  (recur-add-local-vars-to-table table zipper (count table)))

(defn get-symbol-by-name [symbol-name symbol-table]
  (first (filter #(= (:name %) symbol-name) symbol-table)))

(defn get-scope-variable-count [scope symbol-table]
  (count (filter #(= (:scope %) scope) symbol-table)))

(defn create-empty-symbol-table [] [])

(defn add-class-variables [zipper table]
  (recur-add-local-vars-to-table table zipper (count table)))
