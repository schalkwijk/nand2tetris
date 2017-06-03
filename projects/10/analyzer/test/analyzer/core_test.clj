(ns analyzer.core-test
  (:require [analyzer.core :refer :all]
            [clojure.test :refer :all]))

(deftest handling-class-declarations
  (are [value location] (= value (nth (analyze "class Test { }") location))
    {:type :keyword :value "class"} 0
    {:type :identifier :value "Test"} 1
    {:type :symbol :value "{"} 2
    {:type :symbol :value "}"} 3))

(deftest handling-variable-assignment
  (are [value location] (= value (nth (analyze "let i = sum") location))
    {:type :keyword :value "let"} 0
    {:type :identifier :value "i"} 1
    {:type :symbol :value "="} 2
    {:type :identifier :value "sum"} 3))

(deftest handling-variable-assignment-with-function-call
  (are [value location] (= value (nth (analyze "let i = Array.new(length);") location))
    {:type :keyword :value "let"} 0
    {:type :identifier :value "i"} 1
    {:type :symbol :value "="} 2
    {:type :identifier :value "Array"} 3
    {:type :symbol :value "."} 4
    {:type :identifier :value "new"} 5
    {:type :symbol :value "("} 6
    {:type :identifier :value "length"} 7
    {:type :symbol :value ")"} 8
    {:type :symbol :value ";"} 9))

(deftest handling-function-declarations
  (are [value location] (= value (nth (analyze "function void main() { }") location))
    {:type :keyword :value "function"} 0
    {:type :keyword :value "void"} 1
    {:type :identifier :value "main"} 2
    {:type :symbol :value "("} 3
    {:type :symbol :value ")"} 4
    {:type :symbol :value "{"} 5
    {:type :symbol :value "}"} 6))

(deftest handling-function-calling
  (are [value location] (= value (nth (analyze "do Output.printInt(sum / length);") location))
    {:type :keyword :value "do"} 0
    {:type :identifier :value "Output"} 1
    {:type :symbol :value "."} 2
    {:type :identifier :value "printInt"} 3
    {:type :symbol :value "("} 4
    {:type :identifier :value "sum"} 5
    {:type :symbol :value "/"} 6
    {:type :identifier :value "length"} 7
    {:type :symbol :value ")"} 8
    {:type :symbol :value ";"} 9))

(deftest handling-array-accessing
  (are [value location] (= value (nth (analyze "let sum = sum + a[i]") location))
    {:type :keyword :value "let"} 0
    {:type :identifier :value "sum"} 1
    {:type :symbol :value "="} 2
    {:type :identifier :value "sum"} 3
    {:type :symbol :value "+"} 4
    {:type :identifier :value "a"} 5
    {:type :symbol :value "["} 6
    {:type :identifier :value "i"} 7
    {:type :symbol :value "]"} 8))

(deftest handling-integer-constants
  (are [value location] (= value (nth (analyze "let i = 100;") location))
    {:type :keyword :value "let"} 0
    {:type :identifier :value "i"} 1
    {:type :symbol :value "="} 2
    {:type :integerConstant :value "100"} 3
    {:type :symbol :value ";"} 4))

(deftest handling-string-constants
  (are [value location] (= value (nth (analyze "do Output.printString(\"THE AVERAGE IS: \")") location))
    {:type :keyword :value "do"} 0
    {:type :identifier :value "Output"} 1
    {:type :symbol :value "."} 2
    {:type :identifier :value "printString"} 3
    {:type :symbol :value "("} 4
    {:type :stringConstant :value "THE AVERAGE IS: "} 5
    {:type :symbol :value ")"} 6
    {:type :identifier :value ";"} 7))
