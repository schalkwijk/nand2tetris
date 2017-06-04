(ns analyzer.formatter-test
  (:require [analyzer.formatter :refer :all]
            [clojure.test :refer :all]))

(deftest outputting-special-xml-characters
  (are [instruction output]
      (re-find output (with-out-str (output-tokens [instruction])))
    {:type :symbol :value "<"} #"> &lt; <"
    {:type :symbol :value ">"} #"> &gt; <"
    {:type :symbol :value "&"} #"> &amp; <"))
