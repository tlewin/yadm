(ns yadm.dmi)

(defprotocol DMInterface
  (find-where [this dm where-clause options])
  (create! [this dm data options])
  (update! [this dm data options])
  (delete! [this dm entity-id options])
  (update-where! [this dm data where-clause options])
  (delete-where! [this dm where-clause options]))
