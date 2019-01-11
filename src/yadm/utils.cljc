(ns yadm.utils
  (:require [clojure.string :as string]))

(defn collify
  [v]
  (cond
    (nil? v) []
    (coll? v) v
    :else [v]))
