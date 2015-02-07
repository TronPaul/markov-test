(ns markov-text.core
  (:require [clojurewerkz.neocons.rest :as nr]
            [clojurewerkz.neocons.rest.nodes :as nn]
            [clojurewerkz.neocons.rest.relationships :as nrl]
            [clojurewerkz.neocons.rest.constraints :as nc]
            [clojurewerkz.neocons.rest.labels :as nl]
            [clojurewerkz.neocons.rest.cypher :as cy]
            [clojurewerkz.neocons.rest.transaction :as tx]
            [clojure.string :as string]))

(defn ensure-tokens-index
  [conn]
  (nn/create-index conn "tokens" {:unique true :property-keys "text"}))

(defn ensure-token-constraint
  [conn]
  (nc/create-unique conn "Token" :text))

(defn ensure-ngram-constraint
  [conn]
  (nc/create-unique conn "Ngram" :hash))

(defn ensure-ngrams-index
  [conn]
  (nn/create-index conn "nodes" {:unique true :property-keys "hash"}))

(defn- tokenize
  [line]
  (string/split line #" "))

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

(defn- tokens->ngrams
  [line-tokens ngram-size]
  (format-ngrams (partition ngram-size 1 line-tokens)))

(defn- get-or-create-token
  [token conn]
  (nn/create-unique-in-index conn "tokens" "text" token {:text token}))

(defn- get-or-create-ngram
  [ngram conn]
  (nn/create-unique-in-index conn "ngrams" "ngram" (hash ngram) {:hash (hash ngram)}))

(defn- store-links
  [ngram conn]
  (let [ngram-node (get-or-create-ngram (:ngram ngram) conn)
        ngram-token-nodes (map #(get-or-create-token % conn) (:ngram ngram))]
    (if (:prev ngram)
      (nrl/create conn (get-or-create-token (:prev ngram) conn) ngram-node :chain))
    (if (:next ngram)
      (nrl/create conn ngram-node (get-or-create-token (:next ngram) conn) :chain))
    (map #(nrl/create conn % ngram-node :in) ngram-token-nodes)))

(defn- store-chain
  [ngrams conn]
  (map #(store-links % conn) ngrams))

(defn add-line
  [line conn ngram-size]
  (let [line-tokens (tokenize line)
        ngrams (tokens->ngrams line-tokens ngram-size)]
    (doall (store-chain ngrams conn))))
