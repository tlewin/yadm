(defproject yadm "0.1.2"
  :description "Simple Clojure library for data mapping"
  :url "https://github.com/tlewin/yadm"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [inflections "0.13.0"]
                 [honeysql "0.9.4" :scope "provided"]]

  :profiles {:dev {:dependencies [[nrepl "0.4.5"]
                                  [org.clojure/java.jdbc "0.7.8"]
                                  [org.postgresql/postgresql "42.2.4"]]
                   :plugins [[cider/cider-nrepl "0.18.0"]]
                   :repl-options {:init-ns yadm.core}}})
