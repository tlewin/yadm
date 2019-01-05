(ns yadm.core
  (:require [honeysql.format :as sqlf]
            [honeysql.helpers :as sqlh]
            [yadm.db :as ydb]
            [yadm.validation :as yv]
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
  {:validations {}
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

(defrecord SimpleDbInterface [db-conn options]
  ydb/IDbInterface
  (find-where [this dm query])
  (create! [this dm data])
  (update! [this dm entity-id data])
  (delete! [this dm entity-id])
  (update-where! [this dm data where-clause])
  (delete-where! [this dm where-clause]))

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

(defn update!
  [dbi dm entity-id data]
  (-> data
      (execute-function-pipeline
       (flatten [(validation-function-pipeline dm :defined-fields? true)
                 (dm-setting dm :before-update)
                 (fn [value]
                   (update-value (update! dbi dm entity-id value)))
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

(extend-protocol sqlf/ToSql
  DataMapper
  (to-sql [dm]
    (name (dm-setting dm :table))))

(defn- escape-column-name
  [table-name column-name]
  (->> column-name
       (name)
       (str (name table-name) ".")
       (keyword)))

(defn- escape-column-names
  [table-name column-names]
  (let [table-name (name table-name)]
    (map (partial escape-column-name table-name) column-names)))

(defn- build-has-many-stmt
  [owner related options]
  (let [owner-key (or (:owner-key options) :id)
        owner-entity (dm-setting owner :entity-name)
        owner-table (dm-setting owner :table)
        related-key (or (:related-key options)
                        (str (yu/to-snake-case (name owner-entity)) "_id"))
        related-table (dm-setting related :table)]
    (fn [sqlmap]
      (sqlh/left-join sqlmap
                      [related related-table]
                      [:=
                       (escape-column-name owner-table owner-key)
                       (escape-column-name related-table related-key)]))))

(defn- build-association-stmt
  [owner related]
  (let [associations (dm-setting owner :associations)
        owner-entity (dm-setting owner :entity-name)
        owner-table (dm-setting owner :table)
        related-entity (dm-setting related :entity-name)
        related-table (dm-setting related :table)
        [[assoc-type _ & options]] (filter (fn [[x r & _]] (= r related-entity))
                                           associations)
        options (apply hash-map options)]
    (if (nil? assoc-type)
      ;; NOTE: No entity found. Should throw an exception?
      (throw (Exception. (str owner-entity " has no association for " related-entity)))
      (case assoc-type
        :has-many (build-has-many-stmt owner related options)
        (throw (Exception. (str "Unknow association type: " assoc-type)))))))

(sqlh/defhelper include [sqlmap args]
  (let [[related & options] args
        options (apply hash-map options)
        table-name (dm-setting related :table)
        columns (or (:columns options) [:*])
        owner (first (:from sqlmap))
        where (:where options)]
    (assert (datamapper? related))
    (as-> sqlmap m
      ((build-association-stmt owner related) m)
      (apply sqlh/merge-select m (escape-column-names table-name columns))
      (apply sqlh/merge-where m (or where [])))))

(sqlh/defhelper query [sqlmap args]
  (let [[owner & options] args
        options (apply hash-map options)
        table-name (dm-setting owner :table)
        columns (or (:columns options) [:*])]
    (apply sqlh/select
           (sqlh/from owner table-name)
           (escape-column-names table-name columns))))
