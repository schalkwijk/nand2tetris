(ns analyzer.core-test
  (:require [analyzer.core :refer :all]
            [clojure.test :refer :all]))

(deftest outputting-special-xml-characters
  (are [instruction output]
      (re-find output (with-out-str (output-instructions [instruction])))
    {:type :symbol :value "<"} #"> &lt; <"
    {:type :symbol :value ">"} #"> &gt; <"
    {:type :symbol :value "&"} #"> &amp; <"))
