(ns analyzer.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

(defn- split-on-spaces [instruction]
  (str/split instruction #"\s"))

(def keywords
  ["class" "constructor" "function" "method" "field" "static" "var" "int" "char" "boolean" "void" "true" "false" "null" "this" "let" "do" "if" "else" "while" "return"])

(def symbols
  ["{" "}" "(" ")" "[" "]" "." "," ";" "+" "-" "*" "/" "&" "|" "<" ">" "=" "~"])

(defn in?
  "true if collection contains element"
  [collection element]
  (some #(= element %) collection))

(defn- construct-keyword [bit]
  (if (in? keywords bit) {:type :keyword :value bit}))

(defn- construct-symbol [bit]
  (if (in? symbols bit) {:type :symbol :value bit}))

(defn- construct-identifier [bit]
  {:type :identifier :value bit})

(defn- identify-token [bit]
  (first (filter #(not (nil? %)) (map #(% bit) [construct-keyword construct-symbol construct-identifier]))))

(defn analyze [instruction]
  (let [instruction-bits (split-on-spaces instruction)]
    (map identify-token instruction-bits)))

;; (defn -main [file]
;;   (with-open [rdr (clojure.java.io/reader file)]
;;     (dorun (map println (analyze (line-seq rdr) file)))))
