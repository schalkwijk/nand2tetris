(ns compiler.symbol-table
  (:require [clojure.zip :as zip]
            [compiler.zip-helpers :refer :all]))

(defn- add-argument-to-symbol-table [symbol-table zipper count]
  (if (nil? zipper)
    symbol-table
    (let [zipper (if (= (zip/node (zip/down zipper)) ",") (zip/right zipper) zipper)
          {argument-type :value zipper :zipper} (zip-and-apply zipper [])
          {argument-name :value zipper :zipper} (zip-and-apply zipper [zip/right])
          new-count (inc count)]
      (recur (assoc-in symbol-table [:argument new-count] {:name argument-name :type argument-type :position new-count})
             (zip/right zipper) new-count))))

(defn create-symbol-table-for-expression-list [zipper]
  (add-argument-to-symbol-table {:argument []} (zip/down zipper) -1))
