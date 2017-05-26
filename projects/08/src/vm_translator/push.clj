(ns vm-translator.push
  (:require [clojure.string :as str]))

(defn- generate-constant-push-command [value]
  [(str "@" value ) "D=A" "@SP" "A=M" "M=D" "@SP" "M=M+1"])

(defn- generate-relative-push-command [base-address location]
  [(str "@" base-address ) "D=M" (str "@" location) "D=D+A" "A=D" "D=M" "@SP" "A=M" "M=D" "D=A+1" "@SP" "M=D"])

(defn- generate-temp-push-command [location]
  ["@R5" "D=A" (str "@" location) "D=D+A" "A=D" "D=M" "@SP" "A=M" "M=D" "D=A+1" "@SP" "M=D"])

(defn- generate-static-push-command [location context]
  [(str "@" context "." location) "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"])

(defn- generate-pointer-push-command [location]
  [(str "@" (if (= location "0") "THIS" "THAT")) "D=M" "@SP" "A=M" "M=D" "@SP" "M=M+1"])

(defn generate-push-command [memory-segment location context]
  (case memory-segment
    "constant" (generate-constant-push-command location)
    "local" (generate-relative-push-command "LCL" location)
    "argument" (generate-relative-push-command "ARG" location)
    "this" (generate-relative-push-command "THIS" location)
    "that" (generate-relative-push-command "THAT" location)
    "static" (generate-static-push-command location context)
    "pointer" (generate-pointer-push-command location)
    "temp" (generate-temp-push-command location)))
