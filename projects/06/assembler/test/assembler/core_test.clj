(ns assembler.core-test
  (:require  [clojure.test :refer :all]
             [assembler.core :refer :all]))

(deftest assembling-a-instructions
  (is (= "0000000000000010" (:instruction (parse-instruction :raw "@2"))))
  (is (= "0000000000000011" (:instruction (parse-instruction :raw "@3"))))
  (is (= "0000000000000000" (:instruction (parse-instruction :raw "@0")))))

(deftest assembling-c-instructions
  (is (= "1110110000010000" (:instruction (parse-instruction :raw "D=A"))))
  (is (= "1110000010010000" (:instruction (parse-instruction :raw "D=D+A"))))
  (is (= "1110001100001000" (:instruction (parse-instruction :raw "M=D")))))

(deftest assemble-returns-array-of-instructions
  (is (= ["0000000000000010" "1110110000010000" "0000000000000011"] (assemble (seq ["@2" "D=A" "@3"])))))
