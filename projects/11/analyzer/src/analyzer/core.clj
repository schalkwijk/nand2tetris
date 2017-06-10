(ns analyzer.core
  (:gen-class)
  (:require [analyzer.code-generator :refer :all]
            [analyzer.formatter :refer [output-parse-tree]]
            [analyzer.tokenizer :refer :all]
            [analyzer.parser :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- tokenize-parse-compile-and-output-single-file [filename]
  (let [vm-filename (str/replace filename ".jack" ".vm")]
    (with-open [rdr (io/reader filename)]
      (with-open [wrtr (io/writer vm-filename)]
        (.write wrtr (str/join "\n" (compile-code (with-out-str (output-parse-tree (parse-tokens (tokenize-instructions (line-seq rdr))))))))))))

(defn- extract-jack-files-from-directory [directory]
  (filter #(re-matches #".*\.jack" (.getName %)) (.listFiles directory)))

(defn -main [filename]
  (let [file (io/file filename)
        directory? (.isDirectory file)
        files (if directory? (extract-jack-files-from-directory file) [file])]
    (doall (map tokenize-parse-compile-and-output-single-file files))))
