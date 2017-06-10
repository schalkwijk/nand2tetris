(ns compiler.symbol-table-test
  (:require [clojure.test :refer :all]
            [clojure.zip :as zip]
            [compiler.formatter :refer [output-parse-tree]]
            [compiler.parser :refer [parse-tokens]]
            [compiler.symbol-table :refer :all]
            [compiler.tokenizer :refer [tokenize-instructions]]
            [compiler.zip-helpers :refer [zip-str]]))

(defn- prepare-commands-for-symbol-table [instructions]
  (->> instructions
       tokenize-instructions
       parse-tokens
       output-parse-tree
       with-out-str
       zip-str
       zip/down
       zip/right
       zip/right
       zip/right
       zip/right
       zip/node
       zip/xml-zip
       ))

(deftest symbol-table-for-expression-list
  (is (= {:argument [{:name "foo" :type "Fraction", :position 0}
                     {:name "bar" :type "int", :position 1}]}
         (create-symbol-table-for-expression-list (prepare-commands-for-symbol-table ["function int main(Fraction foo, int bar) {}"])))))
