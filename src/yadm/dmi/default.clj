(ns yadm.dmi.default
  (:require [clojure.java.jdbc :as jdbc]
            [inflections.core :as inf]
            [honeysql.format :as sqlf]
            [honeysql.helpers :as sqlh]
            [yadm.core :refer :all]
            [yadm.dmi :refer [DMInterface]]
            [yadm.utils :as yu]))

(defn- map->where-clause
  [m]
  (map (fn [[k v]] [:= k v]) m))

(defn- escape-column-name
  [entity-source column-name]
  (->> column-name
       (name)
       (str (name entity-source) ".")
       (keyword)))

(defn- escape-column-names
  [entity-source column-names]
  (let [entity-source (name entity-source)]
    (map (partial escape-column-name entity-source) column-names)))

(defn- build-has-many-through-stmt
  [sqlmap owner related options]
  (let [owner-key      (or (:owner-key options)
                           (dm-setting owner :primary-key))
        related-key    (or (:related-key options)
                           (dm-setting related :primary-key))
        owner-entity   (dm-setting owner :entity-name)
        related-entity (dm-setting related :entity-name)
        owner-source   (dm-setting owner :entity-source)
        related-source (dm-setting related :entity-source)

        through-source (keyword (:through options))
        ;; NOTE: Should add options for configuring these keys?
        through-okey   (str (inf/underscore (name owner-entity)) "_id")
        through-rkey   (str (inf/underscore (name related-entity)) "_id")]
    (when (or (not= (count owner-key) 1))
      (throw (Exception. (str "No support for compoud primary key: "
                              owner-entity
                              owner-key))))
    (when (or (not= (count related-key) 1))
      (throw (Exception. (str "No support for compoud primary key: "
                              related-entity
                              related-key))))
    (-> sqlmap
        (sqlh/merge-left-join [through-source through-source]
                              [:=
                               (escape-column-name owner-source (first (yu/collify owner-key)))
                               (escape-column-name through-source through-okey)])
        (sqlh/merge-join [related related-source]
                         [:=
                          (escape-column-name through-source through-rkey)
                          (escape-column-name related-source (first (yu/collify related-key)))]))))

(defn- build-has-many-stmt
  [sqlmap owner related options]
  (let [owner-key      (or (:owner-key options) (dm-setting owner :primary-key))
        owner-entity   (dm-setting owner :entity-name)
        owner-source   (dm-setting owner :entity-source)
        related-key    (or (:related-key options)
                           (str (inf/underscore (name owner-entity)) "_id"))
        related-source (dm-setting related :entity-source)]
    (when (or (not= (count owner-key) 1))
      (throw (Exception. (str "No support for compoud primary key: "
                              owner-entity
                              owner-key))))
    (if (contains? options :through)
      (build-has-many-through-stmt sqlmap owner related options)
      (sqlh/merge-left-join sqlmap
                            [related related-source]
                            [:=
                             (escape-column-name owner-source (first (yu/collify owner-key)))
                             (escape-column-name related-source related-key)]))))

(defn- build-belongs-to-stmt
  [sqlmap owner related options]
  (let [related-entity (dm-setting related :entity-name)
        owner-key      (or (:owner-key options)
                           (str (inf/underscore (name related-entity)) "_id"))
        owner-source   (dm-setting owner :entity-source)
        related-key    (or (:related-key options) (dm-setting related :primary-key))
        related-source (dm-setting related :entity-source)]
    (when (or (not= (count related-key) 1))
      (throw (Exception. (str "No support for compoud primary key: "
                              related-entity
                              related-key))))
    (sqlh/merge-left-join sqlmap
                          [related related-source]
                          [:=
                           (escape-column-name owner-source owner-key)
                           (escape-column-name related-source (first (yu/collify related-key)))])))

(defn- build-association-stmt
  [sqlmap owner related]
  (let [associations               (dm-setting owner :associations)
        owner-entity               (dm-setting owner :entity-name)
        related-entity             (dm-setting related :entity-name)
        [[assoc-type _ & options]] (filter (fn [[x r & _]] (= r related-entity))
                                           associations)
        options                    (apply hash-map options)]
    (if (nil? assoc-type)
      ;; NOTE: No entity found. Should throw an exception?
      (throw (Exception. (str owner-entity " has no association for " related-entity)))
      (case assoc-type
        :has-many (build-has-many-stmt sqlmap owner related options)
        :belongs-to (build-belongs-to-stmt sqlmap owner related options)
        (throw (Exception. (str "Unknown association type: " assoc-type)))))))

(extend-protocol sqlf/ToSql
  yadm.core.DataMapper
  (to-sql
    [dm]
    (name (dm-setting dm :entity-source))))

(sqlh/defhelper include [sqlmap args]
  (let [[related & options] args
        options             (apply hash-map options)
        entity-source       (dm-setting related :entity-source)
        columns             (or (:columns options) [:*])
        [[owner]]           (:from sqlmap)]
    (assert (datamapper? related))
    (as-> sqlmap m
      (build-association-stmt m owner related)
      (apply sqlh/merge-select m (escape-column-names entity-source columns)))))

(sqlh/defhelper query [sqlmap args]
  (let [[owner & options] args
        options           (apply hash-map options)
        entity-source     (dm-setting owner :entity-source)
        columns           (or (:columns options) [:*])]
    (apply sqlh/select
           (sqlh/from [owner entity-source])
           (escape-column-names entity-source columns))))

(defrecord DefaultDMI [db-spec options]
  DMInterface
  (find-where
    [this dm where-clause options]
    (jdbc/query (:db-spec this)
                (-> query
                    (sqlf/format))))

  (create!
    [this dm data options]
    (let [[r] (jdbc/insert! (:db-spec this)
                            (dm-setting dm :entity-source)
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
    [this dm data options]
    (let [pk         (dm-setting dm :primary-key)
          pk-data    (select-keys data pk)
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
    [this dm entity-id options]
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
    [this dm data where-clause options]
    (first
     (jdbc/execute! (:db-spec this)
                    (-> (apply sqlh/where where-clause)
                        (sqlh/update dm)
                        (sqlh/sset data)
                        (sqlf/format)))))

  (delete-where!
    [this dm where-clause options]
    (first
     (jdbc/execute! (:db-spec this)
                    (-> (apply sqlh/where where-clause)
                        (sqlh/delete-from dm)
                        (sqlf/format))))))

(defn default-dmi
  ([db-spec]
   (default-dmi db-spec {}))
  ([db-spec options]
   (DefaultDMI. db-spec options)))
