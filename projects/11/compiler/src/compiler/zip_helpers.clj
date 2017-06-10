(ns compiler.zip-helpers
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]))

(defn zip-and-apply [zipper traversal-operations & node-operations]
  (let [node-operations (if node-operations node-operations [zip/down zip/node])
        modified-zipper (reduce #(%2 %1) zipper traversal-operations)]
    {:value (reduce #(%2 %1) modified-zipper node-operations) :zipper modified-zipper}))

(defn consume-content-until-value [value zipper & consumed]
  (let [consumed (if consumed consumed [])
        current-node-content (zip/node (zip/down zipper))]
    (if (= value current-node-content)
      {:zipper (zip/right zipper) :consumed consumed}
      (recur value (zip/right zipper) (conj consumed current-node-content)))))

(defn zip-str [s]
  (zip/xml-zip (xml/parse (java.io.ByteArrayInputStream. (.getBytes s)))))
