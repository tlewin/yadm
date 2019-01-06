(ns yadm.utils-test
  (:require [clojure.test :refer :all]
            [yadm.utils :as u]))

(deftest test-pluralize
  (testing "adds a \"s\" in the end"
    (is (= "products" (u/pluralize "product")))
    (is (= "as" (u/pluralize "a"))))
  (testing "adds \"es\" when word finishes with \"s\""
    (is (= "masses" (u/pluralize "mass")))
    (is (= "gases" (u/pluralize "gas"))))
  (testing "works for non-regular cases"
    (is (= "people" (u/pluralize "person")))))

(deftest test-to-kebab-case
  (is (= "a-test" (u/to-kebab-case "aTest")))
  (is (= "another-big-test" (u/to-kebab-case "AnotherBigTest")))
  (is (= "a-test" (u/to-kebab-case "ATest")))
  (is (= "a" (u/to-kebab-case "A")))
  (is (= "" (u/to-kebab-case ""))))

(deftest test-to-snake-case
  (is (= "a_test" (u/to-snake-case "aTest")))
  (is (= "another_big_test" (u/to-snake-case "AnotherBigTest")))
  (is (= "a_test" (u/to-snake-case "ATest")))
  (is (= "some_kebab_case_string" (u/to-snake-case "some-kebab-case-string")))
  (is (= "a" (u/to-snake-case "A")))
  (is (= "" (u/to-snake-case ""))))

(deftest test-collify
  (is (coll? (u/collify 1)))
  (is (= [] (u/collify nil)))
  (is (= '(1 2 3) (u/collify '(1 2 3))))
  (is (= [:a :b] (u/collify [:a :b]))))
