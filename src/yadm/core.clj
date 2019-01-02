(ns yadm.core
  (:require [honeysql.format :as sqlf]
            [honeysql.helpers :as sqlh]
            [yadm.utils :as u]))

(defrecord DataMapper [settings])

(defn- table-name
  [dm-name]
  (-> dm-name
      (u/to-snake-case)
      (.toLowerCase)
      (u/pluralize)
      (keyword)))

(defn- entity-name
  [dm-name]
  (-> dm-name
      (u/to-kebab-case)
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

(defn find-where
  [db-conn query])

(defn create!
  [db-conn dm data])

(defn update!
  [db-conn dm entity-id data])

(defn delete!
  [db-conn dm entity-id])

(defn update-where!
  [db-conn dm where-clause])

(defn delete-where!
  [db-conn dm where-clause])

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
                        (str (u/to-snake-case (name owner-entity)) "_id"))
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
