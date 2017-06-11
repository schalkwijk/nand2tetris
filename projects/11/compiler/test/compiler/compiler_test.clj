(ns compiler.compiler-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [compiler.compiler :refer :all]
            [compiler.formatter :refer [output-parse-tree]]
            [compiler.parser :refer [parse-tokens]]
            [compiler.tokenizer :refer [tokenize-instructions]]))

(defn- format-tokens-for-compiler [instructions]
  (with-out-str (output-parse-tree (parse-tokens (tokenize-instructions instructions)))))

(deftest simple-seven-program
  (let [instructions ["class Main {" "function void main() {" "do Output.printInt(1 + (2 * 3));" "return;" "}}"]
        output (compile-code (format-tokens-for-compiler instructions))]
    (are [path value] (= (nth output path) value)
        0 "function Main.main 0"
        1 "push constant 1"
        2 "push constant 2"
        3 "push constant 3"
        4 "call Math.multiply 2"
        5 "add"
        6 "call Output.printInt 1"
        7 "pop temp 0"
        8 "push constant 0"
        9 "return")))

(deftest subroutine-variable-dec-and-set
  (let [instructions
        ["class Test {" "function int main() {" "var int value;" "let value = Memory.peek(8000);" "return value;" "}}"]
        output (compile-code (format-tokens-for-compiler instructions))]
    (are [path value] (= (nth output path) value)
      0 "function Test.main 1"
      1 "push constant 8000"
      2 "call Memory.peek 1"
      3 "pop local 0"
      4 "push local 0"
      5 "return")))
