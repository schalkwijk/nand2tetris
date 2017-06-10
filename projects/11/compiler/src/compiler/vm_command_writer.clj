(ns compiler.vm-command-writer
  (:require [clojure.string :as str]))

(defn- output [& bits]
  (str/join " " (flatten bits)))

(defn write-subroutine-declaration [subroutine-type class-name subroutine-name number-of-args]
  (output subroutine-type (str class-name "." subroutine-name) number-of-args))

(defn write-constant-push [constant]
  (output "push" "constant" constant))

(defn write-temp-push [index]
  (output "push" "temp" index))

(defn write-subroutine-call [subroutine-name number-of-args]
  (output "call" subroutine-name number-of-args))

(defn write-operator [operator]
  (output
   (case operator
     "+" "add"
     "-" "sub"
     "*" ["call" "Math.multiply" "2"]
     "/" ["call" "Math.divide" "2"])))
