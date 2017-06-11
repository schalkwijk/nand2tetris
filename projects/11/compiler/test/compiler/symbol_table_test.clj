(ns compiler.symbol-table-test
  (:require [clojure.test :refer :all]
            [clojure.zip :as zip]
            [compiler.formatter :refer [output-parse-tree]]
            [compiler.parser :refer [parse-tokens]]
            [compiler.symbol-table :refer :all]
            [compiler.tokenizer :refer [tokenize-instructions]]
            [compiler.zip-helpers :refer [zip-str]]))

(defn- get-instructions-expression-list [instructions]
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

(defn- get-method-body [instructions]
  (->> instructions
       tokenize-instructions
       parse-tokens
       output-parse-tree
       with-out-str
       zip-str
       zip/down
       zip/right ;; int
       zip/right ;; main
       zip/right ;; open paren
       zip/right ;; expression list
       zip/right ;; close paren
       zip/right ;; routine body
       zip/down ;; routine body
       zip/right ;; skip over {
       ))

(deftest symbol-table-for-expression-list
  (is (= [{:name "foo" :type "Fraction", :position 0 :scope :argument}
          {:name "bar" :type "int", :position 1 :scope :argument}]
         (create-table-for-expression-list (get-instructions-expression-list ["function int main(Fraction foo, int bar) {}"])))))

(deftest symbol-table-for-local-variables
  (is (= [{:name "foo" :type "Fraction", :position 0 :scope :local}
          {:name "bar" :type "int", :position 1 :scope :local}
          {:name "baz" :type "int", :position 2 :scope :local}]
         (:symbol-table (add-local-vars-to-table
           (get-method-body ["function int main() { var Fraction foo; var int bar, baz; }"]) [])))))

(deftest getting-a-symbol-by-name
  (let [symbol-table
        (create-table-for-expression-list (get-instructions-expression-list ["function int main(Fraction foo, int bar) {}"]))]
    (is (= {:name "foo" :type "Fraction", :position 0 :scope :argument}
           (get-symbol-by-name "foo" symbol-table)))))
