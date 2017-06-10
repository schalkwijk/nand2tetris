(ns compiler.formatter-test
  (:require [clojure.test :refer :all]
            [clojure.xml :as xml]
            [compiler.formatter :refer :all]
            [compiler.parser :refer [parse-tokens]]
            [compiler.tokenizer :refer [tokenize-instructions]]))

(defn parse-xml [xml]
  (xml/parse (java.io.ByteArrayInputStream. (.getBytes xml))))

(deftest outputting-special-xml-characters
  (are [instruction output]
      (re-find output (with-out-str (output-tokens [instruction])))
    {:type :symbol :value "<"} #">&lt;<"
    {:type :symbol :value ">"} #">&gt;<"
    {:type :symbol :value "&"} #">&amp;<"))

(deftest outputting-simple-class-parse-tree
  (let [jack-code ["class Main { method void foo() {} }"]
        parse-tree (parse-tokens (tokenize-instructions jack-code))]
    (are [location value]
        (= value (get-in (parse-xml (with-out-str (output-parse-tree parse-tree))) location))
      [:tag] :class
      [:content 0 :tag] :keyword
      [:content 0 :content 0] "class"
      [:content 1 :tag] :identifier
      [:content 1 :content 0] "Main"
      [:content 2 :tag] :symbol
      [:content 2 :content 0] "{"
      [:content 3 :tag] :subroutineDec
      [:content 3 :content 0 :tag] :keyword
      [:content 3 :content 0 :content 0] "method"
      [:content 3 :content 1 :tag] :keyword
      [:content 3 :content 1 :content 0] "void"
      [:content 3 :content 2 :tag] :identifier
      [:content 3 :content 2 :content 0] "foo"
      [:content 3 :content 3 :tag] :symbol
      [:content 3 :content 3 :content 0] "("
      [:content 3 :content 4 :tag] :parameterList
      [:content 3 :content 5 :content 0] ")"
      [:content 3 :content 6 :tag] :subroutineBody
      [:content 3 :content 6 :content 0 :tag] :symbol
      [:content 3 :content 6 :content 0 :content 0] "{"
      [:content 3 :content 6 :content 1 :tag] :statements
      [:content 3 :content 6 :content 1 :content] nil
      [:content 3 :content 6 :content 2 :tag] :symbol
      [:content 3 :content 6 :content 2 :content 0] "}"
      [:content 4 :tag] :symbol
      [:content 4 :content 0] "}")))

(deftest outputting-class-variables
  (let [jack-code ["class Main { field int foo, bar; }"]
        parse-tree (parse-tokens (tokenize-instructions jack-code))]
    (are [location value]
        (= value (get-in (parse-xml (with-out-str (output-parse-tree parse-tree))) location))
      [:content 3 :tag] :classVarDec
      [:content 3 :content 0 :tag] :keyword
      [:content 3 :content 0 :content 0] "field"
      [:content 3 :content 1 :tag] :keyword
      [:content 3 :content 1 :content 0] "int"
      [:content 3 :content 2 :tag] :identifier
      [:content 3 :content 2 :content 0] "foo"
      [:content 3 :content 3 :tag] :symbol
      [:content 3 :content 3 :content 0] ","
      [:content 3 :content 4 :tag] :identifier
      [:content 3 :content 4 :content 0] "bar")))

(deftest outputting-method-variables
  (let [jack-code ["method void foo(int bar, Fraction baz) {}"]
        parse-tree (parse-tokens (tokenize-instructions jack-code))]
    (are [location value]
        (= value (get-in (parse-xml (with-out-str (output-parse-tree parse-tree))) location))
      [:tag] :subroutineDec
      [:content 4 :tag] :parameterList
      [:content 4 :content 0 :tag] :keyword
      [:content 4 :content 0 :content 0] "int"
      [:content 4 :content 1 :tag] :identifier
      [:content 4 :content 1 :content 0] "bar"
      [:content 4 :content 2 :tag] :symbol
      [:content 4 :content 2 :content 0] ","
      [:content 4 :content 3 :tag] :identifier
      [:content 4 :content 3 :content 0] "Fraction"
      [:content 4 :content 4 :tag] :identifier
      [:content 4 :content 4 :content 0] "baz")))
