(defproject rules-engine "0.1.0-SNAPSHOT"
  :author "Chet Mancini"
  :url "http://github.com/chetmancini"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot rules-engine-test.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
