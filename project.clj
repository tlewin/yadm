(defproject yadm "0.1.0-SNAPSHOT"
  :description "Simple Clojure library for data mapping"
  :url "https://github.com/tlewin/yadm"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [honeysql "0.9.4"]]

  :profiles {:dev {:dependencies [[nrepl "0.4.5"]]
                   :plugins [[cider/cider-nrepl "0.18.0"]]
                   :repl-options {:init-ns yadm.core}}})
