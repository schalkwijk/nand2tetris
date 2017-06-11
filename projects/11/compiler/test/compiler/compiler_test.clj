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

(deftest if-else-statements
  (let [instructions ["class Main {" "function int nextMask(int mask) {" "if (mask = 0) {" "return 1;" "}" "else {" "return mask * 2;" "}" "}" "}"]
        output (compile-code (format-tokens-for-compiler instructions))]
    (are [path value] (= (nth output path) value)
      0 "function Main.nextMask 0"
      1 "push argument 0"
      2 "push constant 0"
      3 "eq"
      4 "if-goto IF_TRUE0"
      5 "goto IF_FALSE0"
      6 "label IF_TRUE0"
      7 "push constant 1"
      8 "return"
      9 "goto IF_END0"
      10 "label IF_FALSE0"
      11 "push argument 0"
      12 "push constant 2"
      13 "call Math.multiply 2"
      14 "return"
      15 "label IF_END0")))

(deftest while-statements
  (let [instructions ["class Main {" "function void fillMemory(int startAddress, int length, int value) {" "while (length > 0) {" "do Memory.poke(startAddress, value);" "let length = length - 1;" "let startAddress = startAddress + 1;" "}" "return;" "}" "}"]
        output (compile-code (format-tokens-for-compiler instructions))]
    (are [path value] (= (nth output path) value)
      0 "function Main.fillMemory 0"
      1 "label WHILE_EXP0"
      2 "push argument 1"
      3 "push constant 0"
      4 "gt"
      5 "not"
      6 "if-goto WHILE_END0"
      7 "push argument 0"
      8 "push argument 2"
      9 "call Memory.poke 2"
      10 "pop temp 0"
      11 "push argument 1"
      12 "push constant 1"
      13 "sub"
      14 "pop argument 1"
      15 "push argument 0"
      16 "push constant 1"
      17 "add"
      18 "pop argument 0"
      19 "goto WHILE_EXP0"
      20 "label WHILE_END0"
      21 "push constant 0"
      22 "return")))

(deftest negative-constant
  (let [instructions ["class Main {" "function void fillMemory(int startAddress, int length, int value) {" "do Main.fillMemory(8001, 16, -1);" "return;" "}" "}"]
        output (compile-code (format-tokens-for-compiler instructions))]
    (are [path value] (= (nth output path) value)
      0 "function Main.fillMemory 0"
      1 "push constant 8001"
      2 "push constant 16"
      3 "push constant 1"
      4 "neg"
      5 "call Main.fillMemory 3"
      6 "pop temp 0"
      7 "push constant 0"
      8 "return")))

(deftest fixed-true-constant
  (let [instructions ["class Main {" "function void fillMemory(int startAddress, int length, int value) {" "var boolean loop;" "let loop = true;" "return;" "}" "}"]
        output (compile-code (format-tokens-for-compiler instructions))]
    (are [path value] (= (nth output path) value)
      0 "function Main.fillMemory 1"
      1 "push constant 0"
      2 "not"
      3 "pop local 0"
      4 "push constant 0"
      5 "return")))

(deftest fixed-false-constant
  (let [instructions ["class Main {" "function void fillMemory(int startAddress, int length, int value) {" "var boolean loop;" "let loop = false;" "return;" "}" "}"]
        output (compile-code (format-tokens-for-compiler instructions))]
    (are [path value] (= (nth output path) value)
      0 "function Main.fillMemory 1"
      1 "push constant 0"
      2 "pop local 0"
      3 "push constant 0"
      4 "return")))

(deftest nested-ifs
  (let [instructions ["class Main {" "function void convert(int value) {" "var int mask, position;" "if (~(position > 16)) {" "if (~((value & mask) = 0)) {" "do Memory.poke(8000 + position, 1);" "}" "else {" "do Memory.poke(8000 + position, 0);" "}" "}" "return;" "}" "}"]
        output (compile-code (format-tokens-for-compiler instructions))]
    (are [path value] (= (nth output path) value)
      0 "function Main.convert 2"
      1 "push local 1"
      2 "push constant 16"
      3 "gt"
      4 "not"
      5 "if-goto IF_TRUE0"
      6 "goto IF_FALSE0"
      7 "label IF_TRUE0"
      8 "push argument 0"
      9 "push local 0"
      10 "and"
      11 "push constant 0"
      12 "eq"
      13 "not"
      14 "if-goto IF_TRUE1"
      15 "goto IF_FALSE1"
      16 "label IF_TRUE1"
      17 "push constant 8000"
      18 "push local 1"
      19 "add"
      20 "push constant 1"
      21 "call Memory.poke 2"
      22 "pop temp 0"
      23 "goto IF_END1"
      24 "label IF_FALSE1"
      25 "push constant 8000"
      26 "push local 1"
      27 "add"
      28 "push constant 0"
      29 "call Memory.poke 2"
      30 "pop temp 0"
      31 "label IF_END1"
      32 "goto IF_END0"
      33 "label IF_FALSE0"
      34 "label IF_END0"
      35 "push constant 0"
      36 "return")))
