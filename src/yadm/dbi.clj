(ns yadm.dbi)

(defprotocol DbInterface
  (find-where [this dm query options])
  (create! [this dm data options])
  (update! [this dm data options])
  (delete! [this dm entity-id options])
  (update-where! [this dm data where-clause options])
  (delete-where! [this dm where-clause options]))
