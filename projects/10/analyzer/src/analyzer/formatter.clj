(ns analyzer.formatter
  (:require [clojure.string :as str]))

(def html-escape-sequences
  {\< "&lt;", \> "&gt;", \& "&amp;"})

(defn- format-token [token]
  (let [type (name (:type token))
        value (str/escape (:value token) html-escape-sequences)]
    (str "<" type "> " value " </" type ">")))

(defn output-tokens [tokens]
  (let [formatted-tokens (str/join "\n" (map format-token tokens))]
    (print (str "<tokens>\n" formatted-tokens "\n</tokens>"))))
