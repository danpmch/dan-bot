(defproject dan-bot "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :repositories {"m2-dv8tion" {:url "https://m2.dv8tion.net/releases"}}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [net.dv8tion/JDA "5.0.0-alpha.9"]
                 [com.github.seancorfield/next.jdbc "1.2.772"]
                 [org.xerial/sqlite-jdbc "3.36.0"]
                 [edu.stanford.nlp/stanford-corenlp "4.4.0"]
                 [edu.stanford.nlp/stanford-corenlp "4.4.0" :classifier "models"]]
  :main ^:skip-aot dan-bot.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
