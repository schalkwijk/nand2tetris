(ns vm-translator.pop
  (:require [clojure.string :as str]))

(defn- generate-relative-pop-command [base-address location]
  ["@SP" "M=M-1" (str "@" base-address) "D=M" (str "@" location) "D=D+A" "@R13" "M=D" "@SP" "A=M" "D=M" "@R13" "A=M" "M=D"])

(defn- generate-temp-pop-command [location]
  ["@SP" "M=M-1" "@R5" "D=A" (str "@" location) "D=D+A" "@R13" "M=D" "@SP" "A=M" "D=M" "@R13" "A=M" "M=D"])

(defn- generate-static-pop-command [location context]
  ["@SP" "A=M-1" "D=M" (str "@" context "." location) "M=D" "@SP" "M=M-1"])

(defn- generate-pointer-pop-command [location]
  ["@SP" "M=M-1" "A=M" "D=M" (str "@" (if (= location "0") "THIS" "THAT")) "M=D"])

(defn generate-pop-command [memory-segment location context]
  (case memory-segment
    "local" (generate-relative-pop-command "LCL" location)
    "argument" (generate-relative-pop-command "ARG" location)
    "this" (generate-relative-pop-command "THIS" location)
    "that" (generate-relative-pop-command "THAT" location)
    "static" (generate-static-pop-command location context)
    "pointer" (generate-pointer-pop-command location)
    "temp" (generate-temp-pop-command location)))
