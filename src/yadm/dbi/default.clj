(ns yadm.dbi.default
  (:require [honeysql.format :as sqlf]
            [honeysql.helpers :as sqlh]
            [yadm.core :refer :all]
            [yadm.dbi :refer [IDbInterface]]
            [yadm.utils :as yu]))

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
  (let [owner-key (or (:owner-key options) (dm-setting owner :primary-key))
        owner-entity (dm-setting owner :entity-name)
        owner-table (dm-setting owner :table)
        related-key (or (:related-key options)
                        (str (yu/to-snake-case (name owner-entity)) "_id"))
        related-table (dm-setting related :table)]
    (when (coll? owner-key)
      (throw (Exception. (str "No support for compoud primary key: "
                              owner-entity
                              owner-key))))
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

(extend-protocol sqlf/ToSql
  yadm.core.DataMapper
  (to-sql [dm]
          (name (dm-setting dm :table))))

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

(defrecord DefaultDBI [db-conn options]
  IDbInterface
  (find-where
    [this dm query])

  (create!
    [this dm data])

  (update!
    [this dm data])

  (delete!
    [this dm entity-id])

  (update-where!
    [this dm data where-clause])

  (delete-where!
    [this dm where-clause]))

(defn default-dbi
  [db-conn options]
  (DefaultDBI. db-conn options))