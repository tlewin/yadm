(ns yadm.utils
  (:require [clojure.string :as string]))

(def ^:private plural-exceptions
  {"person" "people"
   "stuff" "stuff"})

;; TODO: This is a silly implementation of a plural function
(defn pluralize
  [^String word]
  (cond
    (contains? plural-exceptions word) (get plural-exceptions word)
    (.endsWith word "s") (str word "es")
    :else (str word "s")))

(defn to-kebab-case
  [^String term]
  (-> term
      (string/replace #"(.+?)([A-Z])" "$1-$2")
      (.toLowerCase)))

(defn to-snake-case
  [^String term]
  (string/replace (to-kebab-case term) #"-" "_"))

(defn collify
  [v]
  (cond
    (nil? v) []
    (coll? v) v
    :else [v]))
