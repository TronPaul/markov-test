(ns markov-text.core-test
  (:require [clojure.test :refer :all]
            [markov-text.core :as core]
            [clojurewerkz.neocons.rest :as nr])
  (:import (markov_text LocalTestServer)))

(defn with-neo4j-server*
  [f & args]
  (let [server (LocalTestServer.)]
    (try
      (.start server)
      (apply f args)
      (finally
        (.stop server)))))

(defmacro with-neo4j-server
  [& body]
  `(with-neo4j-server* (fn [] ~@body)))

(defn- create-connection
  []
  (nr/connect "http://localhost:7474/db/data/"))

(deftest tokens->ngrams-test
  (testing "Create ngrams from tokens list"
    (is (= (seq [{:prev nil :ngram (seq ["The" "quick" "brown"]) :next "fox"}
                 {:prev "The" :ngram (seq ["quick" "brown" "fox"]) :next "jumped"}
                 {:prev "quick" :ngram (seq ["brown" "fox" "jumped"]) :next "over"}
                 {:prev "brown" :ngram (seq ["fox" "jumped" "over"]) :next "the"}
                 {:prev "fox" :ngram (seq ["jumped" "over" "the"]) :next "lazy"}
                 {:prev "jumped" :ngram (seq ["over" "the" "lazy"]) :next "dog."}
                 {:prev "over" :ngram (seq ["the" "lazy" "dog."]) :next nil}])
           (#'core/tokens->ngrams (seq ["The" "quick" "brown" "fox" "jumped" "over" "the" "lazy" "dog."]) 3)))))

(deftest add-line-test
  (testing "Add line to database"
    (with-neo4j-server
      (let [conn (create-connection)]
        (core/ensure-tokens-index conn)
        (core/ensure-ngrams-index conn)
        (core/ensure-token-constraint conn)
        (core/ensure-ngram-constraint conn)
        (core/add-line "The quick brown fox jumped over the lazy dog." conn 3)))))