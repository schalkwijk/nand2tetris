(ns compiler.vm-command-writer
  (:require [clojure.string :as str]))

(defn- output [& bits]
  (str/join " " (flatten bits)))

(defn write-subroutine-declaration [class-name subroutine-name number-of-args]
  (output "function" (str class-name "." subroutine-name) number-of-args))

(defn write-segment-push [segment index]
  (output "push" segment index))

(defn write-constant-push [constant]
  (write-segment-push "constant" constant))

(defn write-temp-push [index]
  (write-segment-push "temp" index))

(defn write-subroutine-call [subroutine-name number-of-args]
  (output "call" subroutine-name number-of-args))

(defn write-segment-pop [segment index]
  (output "pop" segment index))

(defn write-negation-operator []
  (output "neg"))

(defn write-not-operator []
  (output "not"))

(defn write-operator [operator]
  (output
   (case operator
     "+" "add"
     "-" "sub"
     "~" "neg"
     "=" "eq"
     ">" "gt"
     "<" "lt"
     "&" "and"
     "|" "or"
     "*" ["call" "Math.multiply" "2"]
     "/" ["call" "Math.divide" "2"])))
