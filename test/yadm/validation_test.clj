(ns yadm.validation-test
  (:require [clojure.test :refer :all]
            [yadm.validation :as v]))

(def full-validation-map
  {:field1 [[:required]]
   :field2 [[:required]
            [:in :set [:a :b :c]]]
   :field3 [[:format :with #"\S+@\S+"]]
   :field4 [[:range :min 0]]
   :field5 [[:range :max 10]]
   :field6 [[:required]
            [:range :min 0 :min 10]]})

(def simple-validation-map
  {:field1 [[:required]]
   :field2 [[:in :set ["yes" "no"]]]})

(v/defvalidator dummy
  :msg "{value} is not dummy"
  :body
  (fn [params value]
    (= value "dummy")))

(def dummy-validation-map
  {:field1 [[:dummy]]})

(deftest validate
  (testing ":defined-fields? option"
    (is (= {} (v/validate simple-validation-map
                          {:field2 "yes"}
                          :defined-fields? true)))
    (is (= [:field1] (keys (v/validate simple-validation-map
                                       {:field2 "yes"}
                                       :defined-fields? false)))))
  (testing "returns a map"
    (is (map? (v/validate simple-validation-map {:field2 "yes"})))
    (is (map? (v/validate simple-validation-map {:field2 "yes"} :defined-fields? true)))
    (testing "with field as key and [validator msg] as value"
      (let [[field [[validator msg]]] (first
                                       (seq (v/validate simple-validation-map
                                                        {:field2 "invalid"}
                                                        :defined-fields? true)))]
        (is (= field :field2))
        (is (= validator :in))
        (is (string? msg)))))
  (testing "with empty map"
    (is (= {} (v/validate full-validation-map
                          {}
                          :defined-fields? true)))))

(deftest defvalidator
  (testing "returns a default message when fails"
    (let [[field [[validator msg]]] (first
                                     (seq (v/validate dummy-validation-map
                                                      {:field1 "not-dummy"}
                                                      :defined-fields? true)))]
      (is (= msg "not-dummy is not dummy"))))
  (testing "it overwrites the error message"
    (let [[field [[validator msg]]] (first
                                     (seq (v/validate {:field1 [[:dummy :msg "{value} fails"]]}
                                                      {:field1 "not-dummy"}
                                                      :defined-fields? true)))]
      (is (= msg "not-dummy fails")))))

(v/defvalidator skip-nil-validator
  :msg "Skip nil #{value}"
  :skip-nil? true
  :body
  (fn [params value]
    (some? value)))

(v/defvalidator dont-skip-nil-validator
  :msg "Don't skip nil #{value}"
  :skip-nil? false
  :body
  (fn [params value]
    (some? value)))

(defn valid?
  [value validator & vargs]
  (let [r (v/validate {:field [(concat [validator] vargs)]}
                      {:field value})]
    (empty? r)))

(deftest skip-nil-flag
  (is (valid? 1 :skip-nil-validator))
  (is (valid? nil :skip-nil-validator))
  (is (valid? 1 :dont-skip-nil-validator))
  (is (not (valid? nil :dont-skip-nil-validator))))

(deftest required-validator
  (testing "valid values"
    (is (valid? "any string" :required))
    (is (valid? 0 :required))
    (is (valid? [] :required)))
  (testing "non-valid values"
    (is (not (valid? nil :required)))
    (is (not (valid? "" :required)))))

(deftest in-validator
  (testing "valid values"
    (is (valid? :a :in :set [:a :b])))
  (testing "non-valid values"
    (is (not (valid? :c :in :set [:a :b])))))

(deftest format-validator
  (testing "valid values"
    (is (valid? "test@test.com" :format :with #"\S+@\S+\.\S+")))
  (testing "non-valid values"
    (is (not (valid? "non-valid" :format :with #"\S+@\S+\.\S+")))
    (is (not (valid? "non-valid\ntest@test.com" :format :with #"\S+@\S+\.\S+")))))

(deftest range-with-min-validator
  (testing "valid values"
    (is (valid? -1 :range :min -10))
    (is (valid? 5 :range :min 1)))
  (testing "non-valid values"
    (is (not (valid? -1 :range :min 0)))
    (is (not (valid? 5 :range :min 10)))))

(deftest range-with-max-validator
  (testing "valid values"
    (is (valid? -1 :range :max 0))
    (is (valid? 5 :range :max 10)))
  (testing "non-valid values"
    (is (not (valid? -1 :range :max -10)))
    (is (not (valid? 10 :range :max 5)))))

(deftest range-validator
  (testing "with min"
    (testing "valid values"
      (is (valid? -1 :range :min -10))
      (is (valid? 5 :range :min 1)))
    (testing "non-valid values"
      (is (not (valid? -1 :range :min 0)))
      (is (not (valid? 5 :range :min 10)))))
  (testing "with max"
    (testing "valid values"
      (is (valid? -1 :range :max 0))
      (is (valid? 5 :range :max 10)))
    (testing "non-valid values"
      (is (not (valid? -1 :range :max -10)))
      (is (not (valid? 10 :range :max 5)))))
  (testing "with both"
    (testing "valid values"
      (is (valid? 0 :range :min 0 :max 10))
      (is (valid? 10 :range :min 0 :max 10)))
    (testing "non-valid values"
      (is (not (valid? -1 :range :min 0 :max 10)))
      (is (not (valid? 11 :range :min 0 :max 10))))))

(defn custom-fn
  [params value]
  (odd? value))

(deftest custom-validator
  (testing "valid values"
    (is (valid? 1 :custom :fn custom-fn)))
  (testing "non-valid values"
    (is (not (valid? 2 :custom :fn custom-fn)))))
