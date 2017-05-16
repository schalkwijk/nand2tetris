(ns assembler.symbol-table-test
  (:require [assembler.symbol-table :refer :all]
            [clojure.test :refer :all]))

(deftest produces-default-symbol-table
  (is (= "0000000000000000" (get (generate-symbol-table []) "SCREEN")))
  (is (= "0100000000000000" (get (generate-symbol-table []) "KDB")))
  (is (= "0000000000000000" (get (generate-symbol-table []) "SP")))
  (is (= "0000000000000010" (get (generate-symbol-table []) "ARG")))
  (is (= "0000000000000001" (get (generate-symbol-table []) "LCL")))
  (is (= "0000000000000011" (get (generate-symbol-table []) "THIS")))
  (is (= "0000000000000100" (get (generate-symbol-table []) "THAT"))))

(deftest generates-symbols-for-labels
  (is (= "0000000000000000" (get (generate-symbol-table ["(LABEL)" "D=A"]) "LABEL")))
  (is (= "0000000000000010" (get (generate-symbol-table ["D=A" "MD=1" "(LABEL)" "D=-1"]) "LABEL"))))
