(ns yadm.dmi.default
  (:require [clojure.set :as set]
            [clojure.java.jdbc :as jdbc]
            [inflections.core :as inf]
            [honeysql.format :as sqlf]
            [honeysql.helpers :as sqlh]
            [yadm.core :refer :all]
            [yadm.dmi :refer [DMInterface]]
            [yadm.utils :as yu]))

(extend-protocol sqlf/ToSql
  yadm.core.DataMapper
  (to-sql
    [dm]
    (name (dm-setting dm :entity-source))))

(sqlh/defhelper query [sqlmap args]
  (let [[owner & options] args
        options           (apply hash-map options)
        entity-source     (dm-setting owner :entity-source)
        columns           (get options :columns [:*])]
    (apply sqlh/merge-select
           (sqlh/from sqlmap [owner entity-source])
           columns)))

(defn- map->where-clause
  [m]
  (map (fn [[k v]] [:= k v]) m))

(defn- columns-list
  [columns]
  (map (fn [c]
         (if (coll? c)
           ;; NOTE: Pick up the last one which is the alias for the column,
           ;; therefore it's the key name
           (last c)
           c))
       columns))

(defn- has-select*?
  [columns]
  (some #(= :* %) (columns-list columns)))

(defn- escape-columns
  [entity-source columns]
  (map (fn [c]
         (if (coll? c)
           c
           (keyword (str (name entity-source) "." (name c)))))
       columns))

(defn- to-related-key
  ([related-dm]
   (to-related-key related-dm "_id"))
  ([related-dm suffix]
   (let [related-entity (dm-setting related-dm :entity-name)]
     (keyword (str (inf/underscore (name related-entity))
                   suffix)))))

(defn- find-association
  [owner-dm related-dm]
  (let [related-entity (dm-setting related-dm :entity-name)]
    (first
     (filter (fn [[assoc-type r & _]] (= r related-entity))
             (dm-setting owner-dm :associations)))))

(defn- relation-map
  [owner-dm related-dm assoc-type options]
  (if (contains? options :relation-map)
    (:relation-map options)
    (let [owner-entity   (dm-setting owner-dm :entity-name)
          related-entity (dm-setting related-dm :entity-name)]
      (case assoc-type
        :belongs-to {(to-related-key related-dm) :id}
        :has-many (if (contains? options :through)
                    {:id       (to-related-key owner-dm)
                     :-through {(to-related-key related-dm) :id}}
                    {:id (to-related-key owner-dm)})
        :has-one {:id (to-related-key owner-dm)}
        (throw (Exception. (str "Unknown association type: " assoc-type)))))))

(defn- include->association-map
  [owner-dm include]
  (let [[related-dm i-options]   include
        owner-entity             (dm-setting owner-dm :entity-name)
        related-entity           (dm-setting related-dm :entity-name)
        association              (find-association owner-dm related-dm)
        [assoc-type _ a-options] association
        options                  (merge {} ;; Fallback to empty map in case both are nil
                                        a-options
                                        i-options)]
    (if (nil? association)
      (throw (Exception. (str owner-entity " has no association for " related-entity)))
      {:assoc-type   assoc-type
       :related-dm   related-dm
       :options      options
       :as-field     (get options :as related-entity)
       :relation-map (relation-map owner-dm
                                   related-dm
                                   assoc-type
                                   options)})))

(defn- includes->associations-map
  [owner-dm includes]
  (map (partial include->association-map owner-dm)
       includes))

(defn- association-keys
  ([association]
   (association-keys association false))
  ([association reverse?]
   (let [{:keys [relation-map]} association
         relation-map           (dissoc relation-map :-through)
         map-fn                 (if reverse? reverse identity)]
     (map (comp first map-fn) relation-map))))

(defn- ensure-keys
  [columns ensured-keys]
  (if (or (empty? ensured-keys)
          (has-select*? columns))
    columns
    (let [missing-keys (set/difference (set ensured-keys)
                                       (set columns))]
      (concat columns missing-keys))))

(defn- build-association-where-clause
  [data association]
  (let [{:keys [relation-map]} association
        relation-map           (dissoc relation-map :-through)]
    (if (> (count (keys relation-map)) 1)
      ;; TODO: Add support for compound keys
      (throw (Exception. (str "Compound key not implemented association for "
                              (dm-setting (:related-dm association) :entity-name))))
      (let [[owner-key related-key] (first (vec relation-map))]
        [:in related-key (reduce conj
                                 #{}
                                 (map owner-key data))]))))

(defn- build-association-query
  [data association]
  (let [{:keys [assoc-type
                options
                related-dm
                relation-map]} association
        through                (:-through relation-map)
        related-source         (dm-setting related-dm :entity-source)
        columns                (get options :columns [:*])
        columns                (ensure-keys (if (nil? through)
                                              columns
                                              (escape-columns related-source columns))
                                            (association-keys association true))
        query                  (-> (query related-dm :columns columns)
                                   (sqlh/where (build-association-where-clause data association)))]
    (if (nil? through)
      query
      (apply sqlh/join query (get options :through)
             (map->where-clause through)))))

(defn- query-association
  [db-spec data association]
  (jdbc/query db-spec
              (-> (build-association-query data association)
                  (sqlf/format))))

(defn- reshape-association-data
  [association association-map]
  (let [{:keys [assoc-type
                options
                relation-map]} association
        through                (:-through relation-map)
        columns                (get options :columns [:*])
        columns-selector       (when-not (has-select*? columns)
                                 (let [columns-list (columns-list columns)]
                                   (fn [items]
                                     (map #(select-keys % columns-list)
                                          (yu/collify items)))))
        cardinality-selector    (when-not (= assoc-type :has-many)
                                  first)
        through-selector        (when (some? through)
                                  (let [relation-map  (dissoc relation-map :-through)
                                        relation-keys (vals relation-map)]
                                    (fn [items]
                                      (map #(apply dissoc % relation-keys)
                                           (yu/collify items)))))
        selectors               (filter some? [columns-selector
                                               through-selector
                                               cardinality-selector])
        selectors-fn            (apply comp selectors)]
    (if (empty? selectors)
      association-map
      (into {} (map (fn [[k v]]
                      [k (selectors-fn v)])
                    association-map)))))

(defn- join-association
  [db-spec data association]
  (if (empty? data)
    data
    (let [{:keys [assoc-type
                  as-field
                  options
                  related-dm
                  relation-map]} association
          through                (:-through relation-map)
          relation-map           (dissoc relation-map :-through)
          related-keys           (vals relation-map)
          owner-keys             (keys relation-map)
          columns                (get options :columns [:*])
          default-value          (if (= assoc-type :has-many) [] nil)
          association-map        (->> association
                                      (query-association db-spec data)
                                      (group-by (fn [row]
                                                  (vals (select-keys row related-keys))))
                                      (reshape-association-data association))]
      (map (fn [row]
             (let [join-values (vals (select-keys row owner-keys))]
               (assoc row
                      as-field
                      (get association-map join-values default-value))))
           data))))

(defn- query-find-where
  [dmi dm where-clause options]
  (let [columns  (get options :columns  [:*])
        limit    (get options :limit    nil)
        order-by (get options :order-by [])]
    (jdbc/query (:db-spec dmi)
                (cond-> (apply sqlh/where where-clause)
                  true (query dm :columns columns)
                  (and limit
                       (> limit 0)) (sqlh/limit limit)
                  (not-empty order-by) (#(apply sqlh/order-by % order-by))
                  true (sqlf/format)))))

(defn- format-find-where
  [data associations options]
  (let [columns (get options :columns [:*])]
    (if (has-select*? columns)
      data
      (let [columns-list (concat (columns-list columns)
                                 (map :as-field associations))]
        (map (fn [row]
               (select-keys row columns-list))
             data)))))

(defrecord DefaultDMI [db-spec options]
  DMInterface
  (find-where
    [this dm where-clause options]
    (let [columns           (get options :columns  [:*])
          includes          (get options :includes [])
          associations      (includes->associations-map dm includes)
          ;; reduce all association keys into a set
          associations-keys (reduce #(into %1 (association-keys %2))
                                    #{}
                                    associations)
          data              (query-find-where this
                                              dm
                                              where-clause
                                              (assoc options
                                                     :columns
                                                     (ensure-keys
                                                      columns
                                                      associations-keys)))]
      (format-find-where
       (reduce (fn [data association]
                 (join-association (:db-spec this)
                                   data
                                   association))
               data
               associations)
       associations
       options)))

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
