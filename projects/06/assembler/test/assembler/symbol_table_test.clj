(ns assembler.symbol-table-test
  (:require [assembler.symbol-table :refer :all]
            [clojure.test :refer :all]))

(deftest produces-default-symbol-table
  (is (= "0100000000000000" (get (generate-symbol-table ["@SCREEN"]) "@SCREEN")))
  (is (= "0100000000000000" (get (generate-symbol-table ["@KDB"]) "@KDB")))
  (is (= "0000000000000000" (get (generate-symbol-table ["@SP"]) "@SP")))
  (is (= "0000000000000010" (get (generate-symbol-table ["@ARG"]) "@ARG")))
  (is (= "0000000000000001" (get (generate-symbol-table ["@LCL"]) "@LCL")))
  (is (= "0000000000000011" (get (generate-symbol-table ["@THIS"]) "@THIS")))
  (is (= "0000000000001111" (get (generate-symbol-table ["@R15"]) "@R15")))
  (is (= "0000000000000100" (get (generate-symbol-table ["@THAT"]) "@THAT"))))

(deftest generates-symbols-for-labels
  (is (= "0000000000000000" (get (generate-symbol-table ["(LABEL)" "D=A"]) "@LABEL")))
  (is (= "0000000000000010" (get (generate-symbol-table ["D=A" "MD=1" "(LABEL)" "D=-1"]) "@LABEL"))))

(deftest generates-symbols-for-variables
  (is (= "0000000000010000" (get (generate-symbol-table ["@i" "D=A"]) "@i")))
  (is (= "0000000000010000" (get (generate-symbol-table ["@ponggame.moveball$if_true2" "D=A"]) "@ponggame.moveball$if_true2")))
  (is (= "0000000000010001" (get (generate-symbol-table ["@i" "D=A" "@d" "M=-1"]) "@d"))))

(deftest doesnt-clobber-labels-when-used-as-variables
  (is (= "0000000000000000" (get (generate-symbol-table ["(LABEL)" "D=A" "@LABEL" "0;JMP"]) "@LABEL")))
  (is (= "0000000000000010" (get (generate-symbol-table ["@LABEL" "D=A" "(LABEL)" "D=-1"]) "@LABEL")))
  (is (= {"@LABEL" "0000000000000010" "@d" "0000000000010000"}
         (select-keys (generate-symbol-table ["A=M" "@d" "(LABEL)" "D=A" "@LABEL" "0;JMP"]) ["@LABEL" "@d"]))))
