(ns compiler.symbol-table
  (:require [clojure.zip :as zip]
            [compiler.zip-helpers :refer :all]))

(defn- add-argument-to-table [table zipper count]
  (if (nil? zipper)
    table
    (let [zipper (if (= (zip/node (zip/down zipper)) ",") (zip/right zipper) zipper)
          {argument-type :value zipper :zipper} (zip-and-apply zipper [])
          {argument-name :value zipper :zipper} (zip-and-apply zipper [zip/right])]
      (recur (assoc-in table [:argument count] {:name argument-name :type argument-type :position count})
             (zip/right zipper) (inc count)))))

(defn- add-local-var-to-table [table zipper counter]
  (let [type (zip/node (zip/down zipper))
        vars-and-symbols (zip/rights zipper)]
    (:table (reduce #(assoc-in (assoc %1 :counter (inc (:counter %1)))
                               [:table :local (:counter %1)] {:name (first (:content %2)) :type type :position (:counter %1)})
             {:table table :counter counter}
             (into [] (filter #(not (= (:tag %) :symbol)) vars-and-symbols))))))

(defn- recur-add-local-vars-to-table [table zipper counter]
  (if (not (= :varDec (:tag (zip/node zipper))))
    {:symbol-table table :zipper zipper}
    (let [var-zipper (zip/right (zip/down (zip/xml-zip (zip/node zipper))))
          table (add-local-var-to-table table var-zipper counter)]
      (recur table (zip/right zipper) (count (:local table))))))

(defn create-table-for-expression-list [zipper]
  (add-argument-to-table {:argument []} (zip/down zipper) 0))

(defn add-local-vars-to-table [zipper table]
  (recur-add-local-vars-to-table (assoc table :local []) zipper 0))
