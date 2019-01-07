(ns yadm.dbi.default
  (:require [clojure.java.jdbc :as jdbc]
            [honeysql.format :as sqlf]
            [honeysql.helpers :as sqlh]
            [yadm.core :refer :all]
            [yadm.dbi :refer [DbInterface]]
            [yadm.utils :as yu]))

(defn- map->predicate
  [m]
  (if (empty? m)
    []
    (let [p (map (fn [[k v]] [:= k v]) m)
          where-clause (->> p
                         (apply sqlh/where)
                         (sqlf/format))
          [clause & args] where-clause]
      ;; NOTE: A hackish way to get rid off "WHERE " in the beginning of the
      ;; expression
      ;; TODO: Fix it
      (concat [(subs clause 6)] args))))

(defn- map->where-clause
  [m]
  (map (fn [[k v]]
         [:= k v])
       m))

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
    (when (or (not= (count owner-key) 1))
      (throw (Exception. (str "No support for compoud primary key: "
                              owner-entity
                              owner-key))))
    (fn [sqlmap]
      (sqlh/left-join sqlmap
                      [related related-table]
                      [:=
                       (escape-column-name owner-table (first owner-key))
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
        [[owner]] (:from sqlmap)
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
      (sqlh/from [owner table-name])
      (escape-column-names table-name columns))))

(defrecord DefaultDBI [db-spec options]
  DbInterface
  (find-where
    [this dm query]
    (jdbc/query (:db-spec this)
                (-> query
                    (sqlf/format))))

  (create!
    [this dm data]
    (let [[r] (jdbc/insert! (:db-spec this)
                            (dm-setting dm :table)
                            data)
          ;; TODO: Not all drivers returns the generated key.
          ;; (see http://clojure-doc.org/articles/ecosystem/java_jdbc/using_sql.html).
          ;; Should be replaced by another technique?
          rid (:generated-key r)
          ;; TODO: Check the return value for a compound pk
          [pk] (dm-setting dm :primary-key)]
      ;; NOTE: It seems for postgres it returns the entire row
      (if (map? r)
        (merge data r)
        (if (and rid pk)
          (assoc data pk rid)
          data))))

  (update!
    [this dm data]
    (let [pk (dm-setting dm :primary-key)
          pk-data (select-keys data pk)
          nonpk-data (apply dissoc data pk)]
      (when (empty? pk)
        (throw (Exception. (str "Unable to update an entity without PK: "
                                (dm-setting dm :entity-name)))))
      (jdbc/execute! (:db-spec this)
                     (-> (apply sqlh/where (map->where-clause pk-data))
                         (sqlh/update dm)
                         (sqlh/sset nonpk-data)
                         (sqlf/format)))
      data))

  (delete!
    [this dm entity-id]
    (let [pk (dm-setting dm :primary-key)]
      (when (empty? pk)
        (throw (Exception. (str "Unable to delete an entity without PK: "
                                (dm-setting dm :entity-name)))))
      (when-not (has-primary-key? dm entity-id)
        (throw (Exception. (str "entity-id must contain the primary key: "
                                (dm-setting dm :entity-name)
                                " - "
                                (dm-setting dm :primary-key)))))
      (jdbc/execute! (:db-spec this)
                     (-> (apply sqlh/where (map->where-clause entity-id))
                         (sqlh/delete-from dm)
                         (sqlf/format)))))

  (update-where!
    [this dm data where-clause]
    (first
     (jdbc/execute! (:db-spec this)
                    (-> (apply sqlh/where where-clause)
                        (sqlh/update dm)
                        (sqlh/sset data)
                        (sqlf/format)))))

  (delete-where!
    [this dm where-clause]
    (first
     (jdbc/delete! (:db-spec this)
                   (dm-setting dm :table)
                   where-clause))))

(defn default-dbi
  [db-spec options]
  (DefaultDBI. db-spec options))
