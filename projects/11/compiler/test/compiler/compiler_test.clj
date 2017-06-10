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
  (let [expected-output ["function Main.main 0" "push constant 1" "push constant 2" "push constant 3" "call Math.multiply 2" "add" "call Output.printInt 1" "pop temp 0" "push constant 0" "return"]
        instructions ["class Main {" "function void main() {" "do Output.printInt(1 + (2 * 3));" "return;" "}}"]]
    (is (= expected-output (compile-code (format-tokens-for-compiler instructions))))))
