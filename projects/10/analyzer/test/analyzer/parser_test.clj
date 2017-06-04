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
(def method-return-type (create-token :identifier "Fraction"))
(def method-name (create-token :identifier "foo"))
(def int-t (create-token :keyword "int"))
(def void (create-token :keyword "void"))
(def method-parameter (create-token :keyword "y"))

;; class Main { function Fraction foo() {} }
(deftest handling-function-declarations
  (let [tokens [class-t class-name open-curly method method-return-type method-name
                open-paren close-paren open-curly close-curly close-curly]]
    (are [path value] (= value (get-in (vec (parse-tokens tokens)) path))
      [0 :class 0] class-t
      [0 :class 1] class-name
      [0 :class 2] open-curly
      [0 :class 3 :subroutineDec 0] method
      [0 :class 3 :subroutineDec 1] method-return-type
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

;; TODO
;; print basic example to see if you can properly construct XML
;; Handle variable declarations in class
;; Handle method body
