(ns analyzer.core
  (:gen-class)
  (:require [analyzer.formatter :refer [output-tokens]]
            [analyzer.tokenizer :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- tokenize-and-output-single-file [filename]
  (let [xml-filename (str/replace (.getName (io/file filename)) ".jack" "T.xml")
        output-filename (str (System/getProperty "user.dir") "/" xml-filename)]
    (with-open [rdr (io/reader filename)]
      (with-open [wrtr (io/writer output-filename)]
        (.write wrtr (with-out-str (output-tokens (tokenize-instructions (line-seq rdr)))))))))

(defn- extract-jack-files-from-directory [directory]
  (filter #(re-matches #".*\.jack" (.getName %)) (.listFiles directory)))

(defn -main [filename]
  (let [file (io/file filename)
        directory? (.isDirectory file)
        files (if directory? (extract-jack-files-from-directory file) [file])]
    (doall (map tokenize-and-output-single-file files))))
