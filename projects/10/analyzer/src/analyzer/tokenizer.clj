(ns analyzer.tokenizer
  (:require [clojure.string :as str]))

(def keywords
  ["class" "constructor" "function" "method" "field" "static" "var" "int" "char" "boolean" "void" "true" "false" "null" "this" "let" "do" "if" "else" "while" "return"])

(def symbols
  ["{" "}" "(" ")" "[" "]" "." "," ";" "+" "-" "*" "/" "&" "|" "<" ">" "=" "~"])

(def integer-re
  #"[0-9]+(.[0-9]+)?")

(def html-escape-sequences
  {\< "&lt;", \> "&gt;", \& "&amp;"})

(defn- in?
  "True if collection contains element"
  [collection element]
  (some #(= element %) collection))

(defn- first-or-empty [instruction]
  (if (empty? instruction) "" (first instruction)))

(defn- instruction-is-not-comment [instruction]
  (not (-> instruction
           (str/split #"//|/\*\*|\*|\*/")
           first-or-empty
           str/trim
           str/blank?)))

(defn- split-and-strip-instructions [instructions]
  (map #(first (str/split % #"//")) (filter instruction-is-not-comment instructions)))

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

(defn- construct-string [interned-strings bit]
  (if (contains? interned-strings bit) {:type :stringConstant :value (get interned-strings bit)}))

(defn- construct-identifier [bit]
  {:type :identifier :value bit})

(defn- identify-token [bit interned-strings]
  (first (filter some? (map #(% bit) [(partial construct-string interned-strings) construct-keyword construct-symbol construct-integer construct-identifier]))))

(defn- intern-strings [interned-strings instruction]
  (let [match (re-find #"\"(.*?[^\\])\"" instruction)
        replacement-symbol (str "@" (count interned-strings))]
    (if match (recur (assoc interned-strings replacement-symbol (last match)) (str/replace instruction (first match) replacement-symbol))
        {:interned-strings interned-strings :instruction instruction})))

(defn- intern-string-constants [instruction]
  (intern-strings {} instruction))

(defn tokenize-instruction [instruction]
  (let [{interned-strings :interned-strings
         interned-instruction :instruction} (intern-string-constants instruction)
        instruction-bits (split-on-delimiters interned-instruction)]
    (map #(identify-token % interned-strings) instruction-bits)))

(defn tokenize-instructions [instructions]
  (map tokenize-instruction (split-and-strip-instructions instructions)))
