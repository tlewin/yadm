# YADM

A simple Clojure library for data mapping where it's possible to define associations, validations and callbacks.

## Usage

```clojure
(ns example.core
  (:require [yadm.core :as yadm]))

(yadm/defdatamapper Image
  :validations
  {:url [[:required]]})

(yadm/defdatamapper Product
  :validations
  {:name [[:required]]
   :price [[:required]
           [:range :min 0]]
   :status [[:required]
            [:in :set #{"new" "used"}]]}

  :associations
  [[:has-many :image]])

(yadm/defdatamapper User
  :validations
  {:name [[:required]
   :email [[:required]
           [:format :with #"\S+@\S+\.\S+"]]]}

  :associations
  [[:has-many :product]]

  :after-create [send-welcome-email])

(yadm/find-where
  db-conn
  (-> (yadm/query User :columns [:name])
      (yadm/with Product :where [[:= :status "new"]])))

(yadm/create! db-conn User {:name "Test"
                            :email "test@test.com"})
```

## License

Copyright © 2019 Thiago Lewin

Distributed under the Eclipse Public License either version 1.0 or any later version.
