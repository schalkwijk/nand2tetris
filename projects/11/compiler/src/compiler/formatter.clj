(ns compiler.formatter
  (:require [clojure.string :as str]))

(declare output-parse-tree)

(def html-escape-sequences
  {\< "&lt;", \> "&gt;", \& "&amp;"})

(defn- format-token [token]
  (let [type (name (:type token))
        value (str/escape (:value token) html-escape-sequences)]
    (str "<" type ">" value "</" type ">")))

(defn output-tokens [tokens]
  (let [formatted-tokens (str/join "\n" (map format-token tokens))]
    (print (str "<tokens>\n" formatted-tokens "\n</tokens>"))))

(defn- output-intermediary-node [node]
  (let [node-name (first (keys node))]
    (println (str "<" (name node-name) ">"))
    (output-parse-tree (get node node-name))
    (println (str "</" (name node-name) ">"))))

(defn- output-leaf-node [node]
  (println (format-token node)))

(defn- output-parse-tree-node [node]
  (cond
    (vector? node) (output-parse-tree node)
    (nil? (:type node)) (output-intermediary-node node)
    :else (output-leaf-node node)))

(defn output-parse-tree [parse-tree]
  (if (empty? parse-tree) :default
      (let [remainder (rest parse-tree)]
        (output-parse-tree-node (first parse-tree))
        (recur remainder))))
