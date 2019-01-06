(ns yadm.core
  (:require [yadm.validation :as yv]
            [yadm.utils :as yu]))

(defrecord DataMapper [settings])

(defn- table-name
  [dm-name]
  (-> dm-name
      (yu/to-snake-case)
      (.toLowerCase)
      (yu/pluralize)
      (keyword)))

(defn- entity-name
  [dm-name]
  (-> dm-name
      (yu/to-kebab-case)
      (.toLowerCase)
      (keyword)))

(defonce ^:private default-settings
  {:primary-key :id
   :validations {}
   :associations []
   :before-validate []
   :before-create []
   :before-update []
   :before-delete []
   :after-validate []
   :after-create []
   :after-update []
   :after-delete []})

(defn build-dm-settings
  [dm-name settings]
  (-> default-settings
      (assoc :table (table-name dm-name))
      (merge settings)
      (assoc :entity-name (entity-name dm-name))))

(defmacro defdatamapper
  [dm-name & settings]
  `(def ~dm-name
     (DataMapper. (build-dm-settings ~(name dm-name)
                                     ~(apply hash-map settings)))))

(defn datamapper?
  [o]
  (instance? DataMapper o))

(defn dm-setting
  [dm setting]
  (get-in dm [:settings setting]))

(deftype UpdatedValue [new-value])

(defn- updated-value?
  [value]
  (instance? UpdatedValue value))

(defn update-value
  [value]
  (UpdatedValue. value))

(deftype HaltedValue [error])

(defn- halted-execution?
  [value]
  (instance? HaltedValue value))

(defn halt-execution
  [error]
  (HaltedValue. error))

(defn- execute-function-pipeline
  [initial-value fns]
  (reduce
   (fn [value f]
     (let [r (f value)]
       (cond
         (updated-value? r) (.new-value r)
         (halted-execution? r) (reduced r)
         :else value)))
   initial-value
   fns))

(defn find-where
  [dbi dm query]
  (find-where dbi dm query))

(defn- validation-function-pipeline
  [dm & v-options]
  (flatten
   [(dm-setting dm :before-validate)
    (fn [value]
      (let [v (apply yv/validate (flatten [(dm-setting dm :validations)
                                           value
                                           (or v-options [])]))]
        (if (empty? v) ;; No validation error
          v
          (halt-execution [:validation v]))))
    (dm-setting dm :after-validate)]))

(defn- format-pipeline-result
  [r]
  (if (halted-execution? r)
    [:fail nil (.error r)]
    [:ok   r    nil]))

(defn create!
  [dbi dm data]
  (-> data
      (execute-function-pipeline
       (flatten [(validation-function-pipeline dm)
                 (dm-setting dm :before-create)
                 (fn [value]
                   (update-value (create! dbi dm value)))
                 (dm-setting dm :after-create)]))
      (format-pipeline-result)))

(defn has-primary-key?
  [dm data]
  (let [pk (dm-setting dm :primary-key)]
    (every? #(contains? data %)
            (if (coll? pk) pk [pk]))))

(defn update!
  [dbi dm data]
  (when-not (has-primary-key? dm data)
    (throw (Exception. (str "data must contain the primary key: "
                            (dm-setting dm :entity-name)
                            " - "
                            (dm-setting dm :primary-key)))))
  (-> data
      (execute-function-pipeline
       (flatten [(validation-function-pipeline dm :defined-fields? true)
                 (dm-setting dm :before-update)
                 (fn [value]
                   (update-value (update! dbi dm value)))
                 (dm-setting dm :after-update)]))
      (format-pipeline-result)))

(defn delete!
  [dbi dm entity-id]
  (-> entity-id
      (execute-function-pipeline
       (flatten [(dm-setting dm :before-delete)
                 (fn [value]
                   (delete! dbi dm entity-id))
                 (dm-setting dm :after-delete)]))
      (format-pipeline-result)))

(defn update-where!
  [dbi dm data where-clause]
  (dbi update-where! dm data where-clause))

(defn delete-where!
  [dbi dm where-clause]
  (dbi delete-where! dm where-clause))
