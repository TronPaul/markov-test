(defproject markov-text "0.0.3"
  :description "markov text generation library using neo4j"
  :url "http://github.com/TronPaul/markov-text"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clojurewerkz/neocons "3.0.0"]
                 [roul "0.2.0"]]
  :java-source-paths ["src/java"]
  :source-paths ["src/clj"]
  :profiles {:dev {:dependencies [[org.neo4j/neo4j-kernel "2.1.7" :classifier "tests"]
                                  [org.neo4j/neo4j-kernel "2.1.7"]
                                  [org.neo4j.app/neo4j-server "2.1.7"]]
                   :java-source-paths ["test/java"]
                   :test-paths ["test/clj" "test/java"]}})
