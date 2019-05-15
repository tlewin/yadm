(ns yadm.core-test
  (:require [clojure.test :refer :all]
            [yadm.core :refer :all]
            [yadm.dmi :as ydmi]))

(def test-validations
  {:field1 [[:required]]
   :field2 [[:required]
            [:in :set #{"yes" "no"}]]})

(defdatamapper CompoudName
  :validations test-validations)

(defdatamapper CompoudPK
  :primary-key [:type :id])

(deftest test-defdatamapper
  (testing "Validate settings"
    (is (thrown-with-msg? AssertionError
                          #"datamapper settings must be a key/value pair"
                          (eval '(yadm.core/defdatamapper Invalid :a))))
    (is (var? (eval '(yadm.core/defdatamapper Valid :a 1)))))
  (testing "Keep validations"
    (is (= test-validations (dm-setting CompoudName :validations))))
  (testing "Entity source"
    (is (= :compoud_names (dm-setting CompoudName :entity-source))))
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

(defrecord TestDMInterface [return-values]
  ydmi/DMInterface
  (find-where
    [this dm where-clause options]
    (:find-where (:return-values this)))

  (create!
    [this dm data options]
    (merge data
           (:create! (:return-values this))))

  (update!
    [this dm data options]
    (merge data
           (:update! (:return-values this))))

  (delete!
    [this dm entity-id options]
    (merge entity-id
           (:delete! (:return-values this))))

  (update-where!
    [this dm data where-clause options]
    (:update-where! (:return-values this)))

  (delete-where!
    [this dm where-clause options]
    (:delete-where! (:return-values this))))

(defdatamapper Test
  :before-save   [(fn [dm v] (update-value (assoc v :before-save true)))]
  :after-save    [(fn [dm v] (update-value (assoc v :after-save true)))]
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
  (testing "Returns the dmi/find-where return value"
    (is (= [:a :b :c] (find-where (TestDMInterface. {:find-where [:a :b :c]})
                                  Test
                                  {})))))

(deftest test-create!
  (testing "Applies the validations"
    (let [[status _ [error error-msg]] (create! (TestDMInterface. {})
                                                Test
                                                {})]
      (is (= status :fail))
      (is (= error  :validation))))
  (testing "Applies the proper callbacks"
    (let [[status v _] (create! (TestDMInterface. {})
                                Test
                                {:field1 0 :field2 1})]
      (is (= status :ok))
      (is (= v {:field1        0
                :field2        1
                :before-save   true
                :after-save    true
                :before-create true
                :after-create  true})))))

(deftest test-update!
  (testing "Applies the validations"
    (let [[status _ [error error-msg]] (update! (TestDMInterface. {})
                                                Test
                                                {:id 1 :field1 42 :field2 5})]
      (is (= status :fail))
      (is (= error  :validation))))
  (testing "Applies the validations only for defined fields"
    (let [[status _ [error error-msg]] (update! (TestDMInterface. {})
                                                Test
                                                {:id 1 :field2 5})]
      (is (= status :fail))
      (is (= error  :validation))
      (is (= [:field2] (keys error-msg)))))
  (testing "Raises an exception if primary field is not defined"
    (is (thrown-with-msg? Exception
                          #"data must contain the primary key"
                          (update! (TestDMInterface. {})
                                   Test
                                   {:field1 42 :field2 5}))))
  (testing "Applies the proper callbacks"
    (let [[status v _] (update! (TestDMInterface. {})
                                Test
                                {:id 1 :field1 0 :field2 1})]
      (is (= status :ok))
      (is (= v {:id            1
                :field1        0
                :field2        1
                :before-save   true
                :after-save    true
                :before-update true
                :after-update  true})))))

(deftest test-delete!
  (testing "Raises an exception if primary field is not defined"
    (is (thrown-with-msg? Exception
                          #"entity-id must contain the primary key"
                          (delete! (TestDMInterface. {})
                                   Test
                                   {:not-id 1}))))
  (testing "Applies the proper callbacks"
    (let [[status v _] (delete! (TestDMInterface. {})
                                Test
                                {:id 1})]
      (is (= status :ok))
      (is (= v {:id            1
                :before-delete true
                :after-delete  true})))))

(deftest test-update-where!
  (testing "Returns the dmi/update-where! return value"
    (is (= 42 (update-where! (TestDMInterface. {:update-where! 42})
                             Test
                             {:field1 10}
                             [[:>= :field1 5]])))))

(deftest test-delete-where!
  (testing "Returns the dmi/delete-where! return value"
    (is (= 42 (delete-where! (TestDMInterface. {:delete-where! 42})
                             Test
                             {:field1 10}
                             [[:>= :field1 5]])))))

(defdatamapper TestUpdateFieldWith
  :before-create [(update-field-with :field inc)])

(deftest test-update-field-with
  (testing "Updates a given field value with a function"
    (let [[status v _] (create! (TestDMInterface. {})
                                TestUpdateFieldWith
                                {:field 0})]
      (is (= status :ok))
      (is (= v {:field 1}))))

  (testing "Skips the update if field is not present"
    (let [[status v _] (create! (TestDMInterface. {})
                                TestUpdateFieldWith
                                {:other-field 1})]
      (is (= status :ok))
      (is (= v {:other-field 1})))))
