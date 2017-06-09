(ns analyzer.vm-command-writer-test
  (:require [analyzer.vm-command-writer :refer :all]
            [clojure.test :refer :all]))

(deftest writing-function-declaration
  (is (= "function BankAccount.transfer 3\n" (with-out-str (write-subroutine-declaration "function" "BankAccount" "transfer" 3)))))

(deftest writing-constant-push
  (is (= "push constant 5\n" (with-out-str (write-constant-push 5)))))

(deftest writing-temp-push
  (is (= "push temp 5\n" (with-out-str (write-temp-push 5)))))

(deftest writing-function-call
  (is (= "call BankAccount.transfer 3\n" (with-out-str (write-subroutine-call "BankAccount" "transfer" 3)))))

(deftest writing-two-term-operator
  (is (= "add\n" (with-out-str (write-operator "+"))))
  (is (= "sub\n" (with-out-str (write-operator "-"))))
  (is (= "call Math.multiply 2\n" (with-out-str (write-operator "*"))))
  (is (= "call Math.divide 2\n" (with-out-str (write-operator "/")))))
