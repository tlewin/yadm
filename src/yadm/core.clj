(ns yadm.core
  (:require [inflections.core :as inf]
            [yadm.validation :as yv]
            [yadm.utils :as yu]))

(defrecord DataMapper [settings])

(defn- table-name
  [dm-name]
  (-> dm-name
      (inf/underscore)
      (.toLowerCase)
      (inf/plural)
      (keyword)))

(defn- entity-name
  [dm-name]
  (-> dm-name
      (inf/hyphenate)
      (.toLowerCase)
      (keyword)))

(defonce ^:private default-settings
  {:primary-key     :id
   :validations     {}
   :associations    []
   :before-validate []
   :before-create   []
   :before-update   []
   :before-delete   []
   :after-validate  []
   :after-create    []
   :after-update    []
   :after-delete    []})

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
  ([dmi dm query] (find-where dmi dm query {}))
  ([dmi dm query options]
   (.find-where dmi dm query options)))

(defn- validation-function-pipeline
  [dm & v-options]
  (flatten
   [(dm-setting dm :before-validate)
    (fn [dm value]
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
  ([dmi dm data] (create! dmi dm data {}))
  ([dmi dm data options]
   (-> data
     (execute-function-pipeline
      dm
      (flatten [(validation-function-pipeline dm)
                (dm-setting dm :before-create)
                (fn [dm value]
                  (update-value (.create! dmi dm value options)))
                (dm-setting dm :after-create)]))
     (format-pipeline-result))))

(defn has-primary-key?
  [dm data]
  (let [pk (dm-setting dm :primary-key)]
    (every? #(contains? data %) pk)))

(defn update!
  ([dmi dm data] (update! dmi dm data {}))
  ([dmi dm data options]
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
                (fn [dm value]
                  (update-value (.update! dmi dm value options)))
                (dm-setting dm :after-update)]))
     (format-pipeline-result))))

(defn delete!
  ([dmi dm entity-id] (delete! dmi dm entity-id {}))
  ([dmi dm entity-id options]
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
                (fn [dm value]
                  (.delete! dmi dm entity-id options))
                (dm-setting dm :after-delete)]))
     (format-pipeline-result))))

(defn update-where!
  ([dmi dm data where-clause] (update-where! dmi dm data where-clause {}))
  ([dmi dm data where-clause options]
   (.update-where! dmi dm data where-clause options)))

(defn delete-where!
  ([dmi dm where-clause] (delete-where! dmi dm where-clause {}))
  ([dmi dm where-clause options]
   (.delete-where! dmi dm where-clause options)))
