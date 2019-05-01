# YADM

A simple Clojure library for data mapping where it's possible to define associations, validations and callbacks.

## Warning

The current version it's **not** production ready yet and the APIs **might change** in the next interations.

## Build

[![Build Status](https://travis-ci.com/tlewin/yadm.svg?branch=master)](https://travis-ci.org/tlewin/yadm)

## Leiningen

[![Clojars Project](https://img.shields.io/clojars/v/yadm.svg)](https://clojars.org/yadm)

## Usage

```clojure
(ns example.core
  (:require [yadm.core :as yadm]
            [yadm.dmi.default :as ydmi]))

(yadm/defdatamapper User
  :validations
  {:name  [[:required]]
   :email [[:required]
           [:format :with #"\S+@\S+\.\S+"]]}

  :associations
  [[:has-many :product]]

  :after-create [send-welcome-email])

(yadm/defdatamapper Product
  :validations
  {:name      [[:required]]
   :price     [[:required]
               [:range :min 0]]
   :condition [[:required]
               [:in :set #{"new" "used"}]]}

  :associations
  [[:belongs-to :user]])

(def db-spec
  {:classname   "org.postgresql.Driver"
   :subprotocol "postgresql"
   :subname     "//localhost:5432/a_db_name"
   :user        "an-user"
   :password    "a-password"})

(yadm/find-where (ydmi/default-dmi db-spec)
                 User
                 [:= :id 1]
                 {:columns  [:id]
                  :includes [[Product {:as      :items
                                       :columns [:name :price]}]]})

(yadm/create! (ydmi/default-dmi db-spec)
              User {:name  "Test"
                    :email "test@test.com"})
```

## License

Copyright © 2019 Thiago Lewin

Distributed under the Eclipse Public License either version 1.0 or any later version.
