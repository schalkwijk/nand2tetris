(ns analyzer.core-test
  (:require [analyzer.core :refer :all]
            [clojure.test :refer :all]))

(deftest handling-class-declarations
  (are [value location] (= value (nth (analyze-instruction "class Test { }") location))
    {:type :keyword :value "class"} 0
    {:type :identifier :value "Test"} 1
    {:type :symbol :value "{"} 2
    {:type :symbol :value "}"} 3))

(deftest handling-variable-assignment
  (are [value location] (= value (nth (analyze-instruction "let i = sum") location))
    {:type :keyword :value "let"} 0
    {:type :identifier :value "i"} 1
    {:type :symbol :value "="} 2
    {:type :identifier :value "sum"} 3))

(deftest handling-variable-assignment-with-function-call
  (are [value location] (= value (nth (analyze-instruction "let i = Array.new(length);") location))
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
  (are [value location] (= value (nth (analyze-instruction "function void main() { }") location))
    {:type :keyword :value "function"} 0
    {:type :keyword :value "void"} 1
    {:type :identifier :value "main"} 2
    {:type :symbol :value "("} 3
    {:type :symbol :value ")"} 4
    {:type :symbol :value "{"} 5
    {:type :symbol :value "}"} 6))

(deftest handling-function-calling
  (are [value location] (= value (nth (analyze-instruction "do Output.printInt(sum / length);") location))
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
  (are [value location] (= value (nth (analyze-instruction "let sum = sum + a[i]") location))
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
  (are [value location] (= value (nth (analyze-instruction "let i = 100;") location))
    {:type :keyword :value "let"} 0
    {:type :identifier :value "i"} 1
    {:type :symbol :value "="} 2
    {:type :integerConstant :value "100"} 3
    {:type :symbol :value ";"} 4))

(deftest handling-string-constants
  (are [value location] (= value (nth (analyze-instruction "do Output.printString(\"THE AVERAGE IS: \");") location))
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
    (are [value instruction-number token-number]
        (= value (nth (nth (analyze-instructions instructions) instruction-number) token-number))
        {:type :keyword :value "class"} 0 0
        {:type :identifier :value "Main"} 0 1
        {:type :symbol :value "{"} 0 2
        {:type :keyword :value "function"} 1 0
        {:type :keyword :value "void"} 1 1
        {:type :identifier :value "main"} 1 2
        {:type :symbol :value "("} 1 3
        {:type :symbol :value ")"} 1 4
        {:type :symbol :value "{"} 1 5
        {:type :keyword :value "var"} 2 0
        {:type :identifier :value "Array"} 2 1
        {:type :identifier :value "a"} 2 2
        {:type :symbol :value ";"} 2 3
        {:type :symbol :value "}"} 3 0)))

(deftest handling-comments-at-beginning-of-line
  (let [instructions ["//Test", "/** Longer Comment */", "class Main {", "function void main() {", "var Array a;", "}"]]
    (are [value instruction-number token-number]
        (= value (nth (nth (analyze-instructions instructions) instruction-number) token-number))
      {:type :keyword :value "class"} 0 0
      {:type :identifier :value "Main"} 0 1
      {:type :symbol :value "{"} 0 2
      {:type :keyword :value "function"} 1 0
      {:type :keyword :value "void"} 1 1
      {:type :identifier :value "main"} 1 2
      {:type :symbol :value "("} 1 3
      {:type :symbol :value ")"} 1 4
      {:type :symbol :value "{"} 1 5
      {:type :keyword :value "var"} 2 0
      {:type :identifier :value "Array"} 2 1
      {:type :identifier :value "a"} 2 2
      {:type :symbol :value ";"} 2 3
      {:type :symbol :value "}"} 3 0)))

(deftest handling-comments-after-code
  (let [instructions ["class Main {", "function void main() {", " var Array a; // random var", "}"]]
    (is (= 4 (count (nth (analyze-instructions instructions) 2))))))

(deftest outputting-special-xml-characters
  (are [instruction output]
      (re-find output (with-out-str (output-instructions [instruction])))
    {:type :symbol :value "<"} #"> &lt; <"
    {:type :symbol :value ">"} #"> &gt; <"
    {:type :symbol :value "&"} #"> &amp; <"))

;; TODO
;; write Makefile to continually test this MainT.xml files
;; add support for processing a single directory
