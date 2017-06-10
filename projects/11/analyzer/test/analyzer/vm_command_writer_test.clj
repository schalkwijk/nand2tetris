(ns analyzer.vm-command-writer-test
  (:require [analyzer.vm-command-writer :refer :all]
            [clojure.test :refer :all]))

(deftest writing-function-declaration
  (is (= "function BankAccount.transfer 3" (write-subroutine-declaration "function" "BankAccount" "transfer" 3))))

(deftest writing-constant-push
  (is (= "push constant 5" (write-constant-push 5))))

(deftest writing-temp-push
  (is (= "push temp 5" (write-temp-push 5))))

(deftest writing-function-call
  (is (= "call BankAccount.transfer 3" (write-subroutine-call "BankAccount.transfer" 3))))

(deftest writing-two-term-operator
  (is (= "add" (write-operator "+")))
  (is (= "sub" (write-operator "-")))
  (is (= "call Math.multiply 2" (write-operator "*")))
  (is (= "call Math.divide 2" (write-operator "/"))))
