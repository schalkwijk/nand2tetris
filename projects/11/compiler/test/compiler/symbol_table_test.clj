(ns compiler.symbol-table-test
  (:require [clojure.test :refer :all]
            [clojure.zip :as zip]
            [compiler.formatter :refer [output-parse-tree]]
            [compiler.parser :refer [parse-tokens]]
            [compiler.symbol-table :refer :all]
            [compiler.tokenizer :refer [tokenize-instructions]]
            [compiler.zip-helpers :refer [zip-str]]))

(defn- zip-instructions [instructions]
  (->> instructions
       tokenize-instructions
       parse-tokens
       output-parse-tree
       with-out-str
       zip-str))

(defn- get-instructions-expression-list [instructions]
  (->> instructions
       zip-instructions
       zip/down
       zip/right
       zip/right
       zip/right
       zip/right
       zip/node
       zip/xml-zip))

(defn- get-method-body [instructions]
  (->> instructions
       zip-instructions
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

(defn- get-var-dec [instructions]
  (->> instructions
       zip-instructions
       zip/down ;; into class
       zip/right ;; class name
       zip/right ;; open curly
       zip/right ;; var decs
       ))

(deftest symbol-table-for-expression-list
  (is (= [{:name "foo" :type "Fraction", :position 0 :scope :argument}
          {:name "bar" :type "int" :position 1 :scope :argument}]
         (create-table-for-expression-list (get-instructions-expression-list ["function int main(Fraction foo, int bar) {}"])))))

(deftest symbol-table-for-local-variables
  (is (= [{:name "foo" :type "Fraction", :position 0 :scope :local}
          {:name "bar" :type "int" :position 1 :scope :local}
          {:name "baz" :type "int" :position 2 :scope :local}]
         (:symbol-table (add-local-vars-to-table
           (get-method-body ["function int main() { var Fraction foo; var int bar, baz; }"]) [])))))

(deftest local-variables-dont-clobber-args
  (is (= [{:name "foo" :type "Fraction", :position 0 :scope :argument}
          {:name "bar" :type "int" :position 0 :scope :local}
          {:name "baz" :type "int" :position 1 :scope :local}]
         (:symbol-table (add-local-vars-to-table
                         (get-method-body ["function int main(Fraction foo) { var int bar, baz; }"])
                         [{:name "foo" :type "Fraction", :position 0 :scope :argument}])))))

(deftest getting-a-symbol-by-name
  (let [symbol-table
        (create-table-for-expression-list (get-instructions-expression-list ["function int main(Fraction foo, int bar) {}"]))]
    (is (= {:name "foo" :type "Fraction", :position 0 :scope :argument}
           (get-symbol-by-name "foo" symbol-table)))))

(deftest getting-the-number-of-variables-in-scope
  (let [symbol-table
        (create-table-for-expression-list (get-instructions-expression-list ["function int main(Fraction foo, int bar) {}"]))]
    (is (= 2 (get-scope-variable-count :argument symbol-table)))))

(deftest creating-an-empty-symbol-table
  (is (= [] (create-empty-symbol-table))))

(deftest adding-class-variables-to-syntax-table
  (let [var-dec (get-var-dec ["class Main { field int x,y; static Foo z;}"])]
    (is (= [{:name "x" :type "int" :position 0 :scope :this}
            {:name "y" :type "int" :position 1 :scope :this}
            {:name "z" :type "Foo" :position 0 :scope :static}]
           (:symbol-table (add-class-variables var-dec []))))))
