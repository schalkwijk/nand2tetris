(ns analyzer.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [analyzer.tokenizer :refer :all])
  (:gen-class))

(defn- format-instruction [instruction]
  (let [type (name (:type instruction))
        value (str/escape (:value instruction) html-escape-sequences)]
    (str "<" type "> " value " </" type ">")))

(defn output-instructions [instructions]
  (let [formatted-instructions (str/join "\n" (map format-instruction instructions))]
    (print (str "<tokens>\n" formatted-instructions "\n</tokens>"))))

(defn- tokenize-and-output-single-file [filename]
  (let [xml-filename (str/replace (.getName (io/file filename)) ".jack" "T.xml")
        output-filename (str (System/getProperty "user.dir") "/" xml-filename)]
    (with-open [rdr (io/reader filename)]
      (with-open [wrtr (io/writer output-filename)]
        (.write wrtr (with-out-str (output-instructions (flatten (tokenize-instructions (line-seq rdr))))))))))

(defn- extract-jack-files-from-directory [directory]
  (filter #(re-matches #".*\.jack" (.getName %)) (.listFiles directory)))

(defn -main [filename]
  (let [file (io/file filename)
        directory? (.isDirectory file)
        files (if directory? (extract-jack-files-from-directory file) [file])]
    (doall (map tokenize-and-output-single-file files))))
