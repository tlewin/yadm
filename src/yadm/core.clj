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

(defn- reshape-settings
  [settings]
  (assoc settings
         :primary-key
         (yu/collify (:primary-key settings))))

(defn build-dm-settings
  [dm-name settings]
  (-> default-settings
      (assoc :table (table-name dm-name))
      (merge settings)
      (assoc :entity-name (entity-name dm-name))
      (reshape-settings)))

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
  [initial-value dm fns]
  (reduce
   (fn [value f]
     (let [r (f dm value)]
       (cond
         (updated-value? r) (.new-value r)
         (halted-execution? r) (reduced r)
         :else value)))
   initial-value
   fns))

(defn find-where
  ([dbi dm query] (find-where dbi dm query {}))
  ([dbi dm query options]
   (.find-where dbi dm query options)))

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
  ([dbi dm data] (create! dbi dm data {}))
  ([dbi dm data options]
   (-> data
     (execute-function-pipeline
      dm
      (flatten [(validation-function-pipeline dm)
                (dm-setting dm :before-create)
                (fn [value]
                  (update-value (.create! dbi dm value options)))
                (dm-setting dm :after-create)]))
     (format-pipeline-result))))

(defn has-primary-key?
  [dm data]
  (let [pk (dm-setting dm :primary-key)]
    (every? #(contains? data %) pk)))

(defn update!
  ([dbi dm data] (update! dbi dm data {}))
  ([dbi dm data options]
   (when (empty? (dm-setting dm :primary-key))
     (throw (Exception. (str "Unable to update an entity without PK: "
                             (dm-setting dm :entity-name)))))
   (when-not (has-primary-key? dm data)
     (throw (Exception. (str "data must contain the primary key: "
                             (dm-setting dm :entity-name)
                             " - "
                             (dm-setting dm :primary-key)))))
   (-> data
     (execute-function-pipeline
      dm
      (flatten [(validation-function-pipeline dm :defined-fields? true)
                (dm-setting dm :before-update)
                (fn [value]
                  (update-value (.update! dbi dm value)))
                (dm-setting dm :after-update)]))
     (format-pipeline-result))))

(defn delete!
  ([dbi dm entity-id] (delete! dbi dm entity-id {}))
  ([dbi dm entity-id options]
   (when (empty? (dm-setting dm :primary-key))
     (throw (Exception. (str "Unable to delete an entity without PK: "
                             (dm-setting dm :entity-name)))))
   (when-not (has-primary-key? dm entity-id)
     (throw (Exception. (str "entity-id must contain the primary key: "
                             (dm-setting dm :entity-name)
                             " - "
                             (dm-setting dm :primary-key)))))
   (-> entity-id
     (execute-function-pipeline
      dm
      (flatten [(dm-setting dm :before-delete)
                (fn [value]
                  (.delete! dbi dm entity-id))
                (dm-setting dm :after-delete)]))
     (format-pipeline-result))))

(defn update-where!
  ([dbi dm data where-clause] (update-where! dbi dm data where-clause {}))
  ([dbi dm data where-clause options]
   (.update-where! dbi dm data where-clause options)))

(defn delete-where!
  ([dbi dm where-clause] (delete-where! dbi dm where-clause {}))
  ([dbi dm where-clause options]
   (.delete-where! dbi dm where-clause options)))
