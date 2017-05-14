(ns assembler.core-test
  (:require  [clojure.test :refer :all]
             [assembler.core :refer :all]))

(deftest assembling-a-instructions
  (is (= "0000000000000010" (:instruction (parse-instruction :raw "@2"))))
  (is (= "0000000000000011" (:instruction (parse-instruction :raw "@3"))))
  (is (= "0000000000000000" (:instruction (parse-instruction :raw "@0")))))

(deftest assembling-a-instructions-with-builtin-labels
  (is (= "0000000000000000" (:instruction (parse-instruction :raw "@SCREEN"))))
  (is (= "0100000000000000" (:instruction (parse-instruction :raw "@KDB"))))
  (is (= "0000000000000000" (:instruction (parse-instruction :raw "@SP"))))
  (is (= "0000000000000010" (:instruction (parse-instruction :raw "@ARG"))))
  (is (= "0000000000000001" (:instruction (parse-instruction :raw "@LCL"))))
  (is (= "0000000000000011" (:instruction (parse-instruction :raw "@THIS"))))
  (is (= "0000000000000100" (:instruction (parse-instruction :raw "@THAT")))))

(deftest assembling-c-instructions
  (is (= "1110101010001000" (:instruction (parse-instruction :raw "M=0"))))
  (is (= "1110111111010000" (:instruction (parse-instruction :raw "D=1"))))
  (is (= "1111110000010000" (:instruction (parse-instruction :raw "D=M"))))
  (is (= "1110111010100000" (:instruction (parse-instruction :raw "A=-1"))))
  (is (= "1110001100111000" (:instruction (parse-instruction :raw "AMD=D"))))
  (is (= "1110110000101000" (:instruction (parse-instruction :raw "AM=A"))))
  (is (= "1110001111001000" (:instruction (parse-instruction :raw "M=-D"))))
  (is (= "1110110011110000" (:instruction (parse-instruction :raw "AD=-A"))))
  (is (= "1110011111110000" (:instruction (parse-instruction :raw "AD=D+1"))))
  (is (= "1110110111000000" (:instruction (parse-instruction :raw "A+1"))))
  (is (= "1110001110000000" (:instruction (parse-instruction :raw "D-1"))))
  (is (= "1110000010010000" (:instruction (parse-instruction :raw "D=D+A"))))
  (is (= "1110001100001000" (:instruction (parse-instruction :raw "M=D"))))
  (is (= "1110110000010000" (:instruction (parse-instruction :raw "D=A"))))
  (is (= "1111110010000000" (:instruction (parse-instruction :raw "M-1"))))
  (is (= "1111000010000000" (:instruction (parse-instruction :raw "D+M"))))
  (is (= "1110000010000000" (:instruction (parse-instruction :raw "D+A"))))
  (is (= "1111010011000000" (:instruction (parse-instruction :raw "D-M"))))
  (is (= "1111000111000000" (:instruction (parse-instruction :raw "M-D"))))
  (is (= "1111000000000000" (:instruction (parse-instruction :raw "D&M"))))
  (is (= "1111110001000000" (:instruction (parse-instruction :raw "!M"))))
  (is (= "1110000000000000" (:instruction (parse-instruction :raw "D&A"))))
  (is (= "1110010101000000" (:instruction (parse-instruction :raw "D|A"))))
  (is (= "1110110111000100" (:instruction (parse-instruction :raw "A+1;JLT"))))
  (is (= "1110000010010110" (:instruction (parse-instruction :raw "D=D+A;JLE"))))
  (is (= "1110111010100101" (:instruction (parse-instruction :raw "A=-1;JNE"))))
  (is (= "1110000010010011" (:instruction (parse-instruction :raw "D=D+A;JGE"))))
  (is (= "1110000000000010" (:instruction (parse-instruction :raw "D&A;JEQ"))))
  (is (= "1110010101000001" (:instruction (parse-instruction :raw "D|A;JGT"))))
  (is (= "1110001100111111" (:instruction (parse-instruction :raw "AMD=D;JMP")))))

(deftest assemble-returns-array-of-instructions
  (is (= ["0000000000000010" "1110110000010000" "0000000000000011"] (assemble (seq ["@2" "D=A" "@3"])))))

(deftest assemble-totally-ignores-comments
  (is (= ["0000000000000010" "1110110000010000" "0000000000000011"]
         (assemble (seq ["// top level comment " "@2 // comment on line" "D=A" "@3"])))))
