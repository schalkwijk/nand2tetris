(ns analyzer.core-test
  (:require [analyzer.core :refer :all]
            [clojure.test :refer :all]))

(deftest handling-class-declarations
  (are [value location] (= value (nth (analyze "class Test { }") location))
    {:type :keyword :value "class"} 0
    {:type :identifier :value "Test"} 1
    {:type :symbol :value "{"} 2
    {:type :symbol :value "}"} 3))
