(ns yadm.core-test
  (:require [clojure.test :refer :all]
            [yadm.core :refer :all]
            [yadm.dbi :as ydbi]))

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
    (is (= [:id] (dm-setting CompoudName :primary-key)))
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

(defrecord TestDBInterface [return-values]
  ydbi/DbInterface
  (find-where
    [this dm query options]
    (:find-where (:return-values this)))

  (create!
    [this dm data options]
    (:create! (:return-values this)))

  (update!
    [this dm data options]
    (:update! (:return-value this)))

  (delete!
    [this dm entity-id options]
    (:delete! (:return-value this)))

  (update-where!
    [this dm data where-clause options]
    (:update-where! (:return-value this)))

  (delete-where!
    [this dm where-clause options]
    (:delete-where! (:return-value this))))

(defdatamapper Test
  :before-create [(fn [dm v] (update-value (assoc v :before-create true)))]
  :after-create  [(fn [dm v] (update-value (assoc v :after-create true)))]
  :before-update [(fn [dm v] (update-value (assoc v :before-update true)))]
  :after-update  [(fn [dm v] (update-value (assoc v :after-update true)))]
  :before-delete [(fn [dm v] (update-value (assoc v :before-delete true)))]
  :after-delete  [(fn [dm v] (update-value (assoc v :after-delete true)))]
  :validations
  {:field1 [[:required]]
   :field2 [[:required]
            [:range :min 0 :max 1]]})

(deftest test-find-where
  (testing "Returns the dbi/find-where return value"
    (is (= [:a :b :c] (find-where (TestDBInterface. {:find-where [:a :b :c]})
                                  Test
                                  {})))))

(deftest test-create!
  (testing "Applies the validations"
    (let [[status _ [error error-msg]] (create! (TestDBInterface. {})
                                                Test
                                                {})]
      (is (= status :fail))
      (is (= error  :validation)))))

(deftest test-update!
  (testing "Applies the validations"
    (let [[status _ [error error-msg]] (update! (TestDBInterface. {})
                                                Test
                                                {:id 1
                                                 :field1 42
                                                 :field2 5})]
      (is (= status :fail))
      (is (= error  :validation))))
  (testing "Applies the validations only for defined fields"
    (let [[status _ [error error-msg]] (update! (TestDBInterface. {})
                                                Test
                                                {:id 1
                                                 :field2 5})]
      (is (= status :fail))
      (is (= error  :validation))
      (is (= [:field2] (keys error-msg)))))
  (testing "Raises an exception if primary field is not defined"
    (is (thrown-with-msg? Exception
                          #"data must contain the primary key"
                          (update! (TestDBInterface. {})
                                   Test
                                   {:field1 42
                                    :field2 5})))))
