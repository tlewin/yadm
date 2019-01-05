(ns yadm.db)

(defprotocol IDbInterface
  (find-where [this dm query])
  (create! [this dm data])
  (update! [this dm data])
  (delete! [this dm entity-id])
  (update-where! [this dm data where-clause])
  (delete-where! [this dm where-clause]))
