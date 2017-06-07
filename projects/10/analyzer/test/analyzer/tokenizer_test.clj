(ns analyzer.tokenizer-test
  (:require [analyzer.tokenizer :refer :all]
            [clojure.test :refer :all]))

(deftest handling-class-declarations
  (are [value location] (= value (nth (tokenize-instruction "class Test { }") location))
    {:type :keyword :value "class"} 0
    {:type :identifier :value "Test"} 1
    {:type :symbol :value "{"} 2
    {:type :symbol :value "}"} 3))

(deftest handling-variable-assignment
  (are [value location] (= value (nth (tokenize-instruction "let i = sum") location))
    {:type :keyword :value "let"} 0
    {:type :identifier :value "i"} 1
    {:type :symbol :value "="} 2
    {:type :identifier :value "sum"} 3))

(deftest handling-variable-assignment-with-function-call
  (are [value location] (= value (nth (tokenize-instruction "let i = Array.new(length);") location))
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
  (are [value location] (= value (nth (tokenize-instruction "function void main() { }") location))
    {:type :keyword :value "function"} 0
    {:type :keyword :value "void"} 1
    {:type :identifier :value "main"} 2
    {:type :symbol :value "("} 3
    {:type :symbol :value ")"} 4
    {:type :symbol :value "{"} 5
    {:type :symbol :value "}"} 6))

(deftest handling-function-calling
  (are [value location] (= value (nth (tokenize-instruction "do Output.printInt(sum / length);") location))
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
  (are [value location] (= value (nth (tokenize-instruction "let sum = sum + a[i]") location))
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
  (are [value location] (= value (nth (tokenize-instruction "let i = 100;") location))
    {:type :keyword :value "let"} 0
    {:type :identifier :value "i"} 1
    {:type :symbol :value "="} 2
    {:type :integerConstant :value "100"} 3
    {:type :symbol :value ";"} 4))

(deftest handling-negative-integer-constants
  (are [value location] (= value (nth (tokenize-instruction "let i = -100;") location))
    {:type :symbol :value "-"} 3
    {:type :integerConstant :value "100"} 4))

(deftest handling-string-constants
  (are [value location] (= value (nth (tokenize-instruction "do Output.printString(\"THE AVERAGE IS: \");") location))
    {:type :keyword :value "do"} 0
    {:type :identifier :value "Output"} 1
    {:type :symbol :value "."} 2
    {:type :identifier :value "printString"} 3
    {:type :symbol :value "("} 4
    {:type :stringConstant :value "THE AVERAGE IS: "} 5
    {:type :symbol :value ")"} 6
    {:type :symbol :value ";"} 7))

(deftest handling-multiline-entries
  (let [instructions ["class Main {", "function void main() {", " var Array a;", "}"]]
    (are [value token-number]
        (= value (nth (tokenize-instructions instructions) token-number))
        {:type :keyword :value "class"} 0
        {:type :identifier :value "Main"} 1
        {:type :symbol :value "{"} 2
        {:type :keyword :value "function"} 3
        {:type :keyword :value "void"} 4
        {:type :identifier :value "main"} 5
        {:type :symbol :value "("} 6
        {:type :symbol :value ")"} 7
        {:type :symbol :value "{"} 8
        {:type :keyword :value "var"} 9
        {:type :identifier :value "Array"} 10
        {:type :identifier :value "a"} 11
        {:type :symbol :value ";"} 12
        {:type :symbol :value "}"} 13)))

(deftest handling-comments-at-beginning-of-line
  (let [instructions ["//Test", "/** Longer Comment */", "class Main {", "function void main() {", "var Array a;", "}"]]
    (are [value token-number]
        (= value (nth (tokenize-instructions instructions) token-number))
      {:type :keyword :value "class"} 0
      {:type :identifier :value "Main"} 1
      {:type :symbol :value "{"} 2
      {:type :keyword :value "function"} 3
      {:type :keyword :value "void"} 4
      {:type :identifier :value "main"} 5
      {:type :symbol :value "("} 6
      {:type :symbol :value ")"} 7
      {:type :symbol :value "{"} 8
      {:type :keyword :value "var"} 9
      {:type :identifier :value "Array"} 10
      {:type :identifier :value "a"} 11
      {:type :symbol :value ";"} 12
      {:type :symbol :value "}"} 13)))

(deftest handling-comments-after-code
  (let [instructions ["class Main {", "function void main() {", " var Array a; // random var", "}"]]
    (is (= 14 (count (tokenize-instructions instructions))))))
