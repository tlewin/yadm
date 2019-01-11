(ns yadm.utils-test
  (:require [clojure.test :refer :all]
            [yadm.utils :as u]))

(deftest test-collify
  (is (coll? (u/collify 1)))
  (is (= [] (u/collify nil)))
  (is (= '(1 2 3) (u/collify '(1 2 3))))
  (is (= [:a :b] (u/collify [:a :b]))))
