(ns analyzer.parser-test
  (:require  [analyzer.parser :refer :all]
             [clojure.test :refer :all]))

(defn- create-token [type value]
  {:type type :value value})

(def class-t (create-token :keyword "class"))
(def class-name (create-token :identifier "Main"))
(def open-curly (create-token :symbol "{"))
(def close-curly (create-token :symbol "}"))
(def open-paren (create-token :symbol "("))
(def close-paren (create-token :symbol ")"))
(def method (create-token :keyword "method"))
(def fraction (create-token :identifier "Fraction"))
(def method-name (create-token :identifier "foo"))
(def int-t (create-token :keyword "int"))
(def void (create-token :keyword "void"))
(def method-parameter (create-token :keyword "y"))
(def semicolon (create-token :symbol ";"))
(def static (create-token :keyword "static"))
(def field (create-token :keyword "field"))
(def foo (create-token :identifier "foo"))
(def bar (create-token :identifier "bar"))
(def baz (create-token :identifier "foo"))
(def comma (create-token :symbol ","))

;; class Main { function Fraction foo() {} }
(deftest handling-function-declarations
  (let [tokens [class-t class-name open-curly method fraction method-name
                open-paren close-paren open-curly close-curly close-curly]]
    (are [path value] (= value (get-in (vec (parse-tokens tokens)) path))
      [0 :class 0] class-t
      [0 :class 1] class-name
      [0 :class 2] open-curly
      [0 :class 3 :subroutineDec 0] method
      [0 :class 3 :subroutineDec 1] fraction
      [0 :class 3 :subroutineDec 2] method-name
      [0 :class 3 :subroutineDec 3] open-paren
      [0 :class 3 :subroutineDec 4] close-paren
      [0 :class 3 :subroutineDec 5] open-curly
      [0 :class 3 :subroutineDec 6] close-curly
      [0 :class 4] close-curly)))

;; class Main { function void foo() {} }
(deftest handling-void-function-returns
  (let [tokens [class-t class-name open-curly method void method-name
                open-paren close-paren open-curly close-curly close-curly]]
    (are [path value] (= value (get-in (vec (parse-tokens tokens)) path))
      [0 :class 3 :subroutineDec 0] method
      [0 :class 3 :subroutineDec 1] void
      [0 :class 3 :subroutineDec 2] method-name)))

;; class Main { function int foo() {} }
(deftest handling-built-in-type-function-returns
  (let [tokens [class-t class-name open-curly
                method int-t method-name open-paren close-paren open-curly close-curly
                close-curly]]
    (are [path value] (= value (get-in (vec (parse-tokens tokens)) path))
      [0 :class 3 :subroutineDec 0] method
      [0 :class 3 :subroutineDec 1] int-t
      [0 :class 3 :subroutineDec 2] method-name)))

;; class Main { field Fraction baz; static int foo, bar; }
(deftest handling-class-level-variable-declarations
  (let [tokens [class-t class-name open-curly
                field fraction baz semicolon
                static int-t foo comma bar semicolon
                close-curly]]
    (are [path value] (= value (get-in (vec (parse-tokens tokens)) path))
      [0 :class 3 :classVarDec 0] field
      [0 :class 3 :classVarDec 1] fraction
      [0 :class 3 :classVarDec 2] baz
      [0 :class 3 :classVarDec 3] semicolon
      [0 :class 4 :classVarDec 0] static
      [0 :class 4 :classVarDec 1] int-t
      [0 :class 4 :classVarDec 2] foo
      [0 :class 4 :classVarDec 3] comma
      [0 :class 4 :classVarDec 4] bar
      [0 :class 4 :classVarDec 5] semicolon)))

;; TODO
;; handle method arguments
;; Handle method body
