(ns analyzer.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

(def keywords
  ["class" "constructor" "function" "method" "field" "static" "var" "int" "char" "boolean" "void" "true" "false" "null" "this" "let" "do" "if" "else" "while" "return"])

(def symbols
  ["{" "}" "(" ")" "[" "]" "." "," ";" "+" "-" "*" "/" "&" "|" "<" ">" "=" "~"])

(def integer-re
  #"[0-9]+(.[0-9]+)?")

(defn- in?
  "true if collection contains element"
  [collection element]
  (some #(= element %) collection))

(defn- void-matcher [pattern]
  (str "(?=\\" pattern ")|(?<=\\" pattern ")"))

(defn- split-on-delimiters [instruction]
  (filter #(not (str/blank? %)) (str/split instruction (re-pattern (str/join "|" (conj (map void-matcher symbols) "\\s"))))))

(defn- construct-keyword [bit]
  (if (in? keywords bit) {:type :keyword :value bit}))

(defn- construct-symbol [bit]
  (if (in? symbols bit) {:type :symbol :value bit}))

(defn- construct-integer [bit]
  (if (re-matches integer-re bit) {:type :integerConstant :value bit}))

(defn- construct-identifier [bit]
  {:type :identifier :value bit})

(defn- identify-token [bit]
  (first (filter #(not (nil? %)) (map #(% bit) [construct-keyword construct-symbol construct-integer construct-identifier]))))

(defn analyze [instruction]
  (let [{interned-strings :interned-strings
         interned-instruction :interned-instruction} (intern-string-constants instruction)
        instruction-bits (split-on-delimiters interned-instruction)]
    (map identify-token instruction-bits)))

;; (defn -main [file]
;;   (with-open [rdr (clojure.java.io/reader file)]
;;     (dorun (map println (analyze (line-seq rdr) file)))))

;; (defn- intern-string [string-to-intern instruction]
;;   )

;; (defn- intern-string-constants [instruction]
;;   (reduce #(intern-string %2 %1) {:interned-strings {} :interned-instruction instruction} (or (re-seq #"\"(.*?[^\\])\"" instruction) [])))
