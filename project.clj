(defproject lang "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [net.sourceforge.parallelcolt/parallelcolt "0.10.0"]
                 [org.clojure/clojure "1.6.0"]
                 [clucy "0.4.0"]
                 [instaparse "1.3.3"]
                 [org.clojure/core.match "0.2.1"]
                 [org.clojure/data.xml "0.0.8"]
                 [org.clojure/data.zip "0.1.1"]
                 ]
  :main lang.benchmarks.wikismoonr
  :jvm-opts ["-Xmx7g" "-server"] 
  )
