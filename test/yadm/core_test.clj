(ns yadm.core-test
  (:require [clojure.test :refer :all]
            [yadm.core :refer :all]))

(def test-validations
  {:field1 [[:required]]
   :field2 [[:required]
            [:in :set #{"yes" "no"}]]})

(defdatamapper CompoudName
  :validations test-validations)

(defdatamapper CompoudPK
  :primary-key [:type :id])

(deftest test-defdatamapper
  (testing "Keep validations"
    (is (= test-validations (dm-setting CompoudName :validations))))
  (testing "Table name"
    (is (= :compoud_names (dm-setting CompoudName :table))))
  (testing "Entity name"
    (is (= :compoud-name (dm-setting CompoudName :entity-name))))
  (testing "Primary key"
    (is (= :id (dm-setting CompoudName :primary-key)))
    (is (= [:type :id] (dm-setting CompoudPK :primary-key)))))

(deftest test-datamapper?
  (testing "Returns true when it's a datamapper"
    (is (datamapper? CompoudName)))
  (testing "Returns false when it's not a datamapper"
    (is (not (datamapper? nil)))
    (is (not (datamapper? {})))
    (is (not (datamapper? [])))))

(deftest test-dm-setting
  (testing "Returns a given setting, if it exists, nil otherwise"
    (is (= test-validations (dm-setting CompoudName :validations)))
    (is (nil? (dm-setting CompoudName :nonexistent)))))

(defdatamapper S)
(defdatamapper T
  :primary-key [:type :id])

(deftest test-has-primary-key?
  (is (has-primary-key? S {:id 1}))
  (is (has-primary-key? T {:type :a :id 1}))
  (is (not (has-primary-key? S {:email "test@test.com"})))
  (is (not (has-primary-key? T {:id 1})))
  (is (not (has-primary-key? T {:type :a})))
  (is (not (has-primary-key? T {:email "test@test.com"}))))
