(defproject com.inferstructure/repl "0.1.0-SNAPSHOT"
  :description "Some REPL help"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]]
  :profiles {:test {:dependencies [[org.clojure/test.check "0.7.0"]]}
             :dev {:plugins [[lein-codox "0.9.1"]]}})
