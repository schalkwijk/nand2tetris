(ns compiler.vm-command-writer-test
  (:require [clojure.test :refer :all]
            [compiler.vm-command-writer :refer :all]))

(deftest writing-function-declaration
  (is (= "function BankAccount.transfer 3" (write-subroutine-declaration "function" "BankAccount" "transfer" 3))))

(deftest writing-constant-push
  (is (= "push constant 5" (write-constant-push 5))))

(deftest writing-temp-push
  (is (= "push temp 5" (write-temp-push 5))))

(deftest writing-segment-push
  (is (= "push segment 5" (write-segment-push "segment" 5))))

(deftest writing-function-call
  (is (= "call BankAccount.transfer 3" (write-subroutine-call "BankAccount.transfer" 3))))

(deftest writing-two-term-operator
  (is (= "add" (write-operator "+")))
  (is (= "sub" (write-operator "-")))
  (is (= "call Math.multiply 2" (write-operator "*")))
  (is (= "call Math.divide 2" (write-operator "/"))))

(deftest writing-segment-pop
  (is (= "pop segment 5" (write-segment-pop "segment" 5))))
