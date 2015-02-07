(ns markov-text.core
  (:require [clojurewerkz.neocons.rest :as nr]
            [clojurewerkz.neocons.rest.nodes :as nn]
            [clojurewerkz.neocons.rest.relationships :as nrl]
            [clojurewerkz.neocons.rest.constraints :as nc]
            [clojurewerkz.neocons.rest.labels :as nl]
            [clojurewerkz.neocons.rest.cypher :as cy]
            [clojure.string :as string]))

(defn- format-ngrams
  "Make ngrams list into ({:prev token :ngram ngram :next token}...)"
  [ngrams]
  (loop [prev nil
         coll ngrams
         acc nil]
    (if (empty? coll)
      acc
      (let [ngram (first coll)
            next-prev (first ngram)
            next-coll (rest coll)
            next (last (first next-coll))]
        (recur next-prev next-coll (concat acc [{:prev prev :ngram ngram :next next}]))))))

(defn- ngram-tokens
  [line-tokens ngram-size]
  (format-ngrams (partition ngram-size 1 line-tokens)))

(defn- tokenize
  [line]
  (string/split #" " line))

(defn add-line
  [line conn ngram-size]
  (let [line-tokens (tokenize line)
        ngrams (ngram-tokens line-tokens ngram-size)]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
