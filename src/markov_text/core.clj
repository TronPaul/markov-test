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
  (let [token-node (nn/create-unique-in-index conn "tokens" "text" token {:text token})]
    (nl/add conn token-node "Token")
    token-node))

(defn- get-or-create-ngram
  ([ngram conn]
    (get-or-create-ngram ngram conn {}))
  ([ngram conn {:keys [start end] :or {start false end false}}]
    (let [ngram-node (nn/create-unique-in-index conn "ngrams" "ngram" (hash ngram) {:hash (hash ngram) :tokens ngram :start start :end end})]
      (nl/add conn ngram-node "Ngram")
      ngram-node)))

(defn- store-links
  [ngram conn]
  (let [ngram-node (get-or-create-ngram (:ngram ngram) conn {:start (nil? (:prev ngram)) :end (nil? (:next ngram))})
        ngram-token-nodes (map #(get-or-create-token % conn) (:ngram ngram))]
    (if (:prev ngram)
      (nrl/create conn (get-or-create-token (:prev ngram) conn) ngram-node :chain))
    (if (:next ngram)
      (nrl/create conn ngram-node (get-or-create-token (:next ngram) conn) :chain))
    (doall (map #(nrl/create conn % ngram-node :in) ngram-token-nodes))))

(defn- store-chain
  [ngrams conn]
  (map #(store-links % conn) ngrams))

(defn add-line
  [line conn ngram-size]
  (let [line-tokens (tokenize line)
        ngrams (tokens->ngrams line-tokens ngram-size)]
    (doall (store-chain ngrams conn))))

(defn- ngram->prevs
  [ngram-node conn]
  (first (:data (cy/query conn "MATCH (target:Ngram)<-[:chain*1..1]-(prev:Token) WHERE id(target) = {id} RETURN prev" {:id (get-in ngram-node [:metadata :id])}))))

(defn- ngram->nexts
  [ngram-node conn]
  (first (:data (cy/query conn "MATCH (target:Ngram)-[:chain*1..1]->(next:Token) WHERE id(target) = {id} RETURN next" {:id (get-in ngram-node [:metadata :id])}))))

(defn- random-token
  [conn]
  (rand-nth (nl/get-all-nodes conn "Token")))

(defn- build-directional
  [ngram-node token-getter terminate-pred ngram+token->ngram token-joiner conn]
  (loop [ngram-node ngram-node
         token-acc nil]
    (let [token-nodes (token-getter ngram-node conn)]
      (if (empty? token-nodes)
        token-acc
        (let [token-node (rand-nth token-nodes)
              next-ngram-node (ngram+token->ngram ngram-node token-node)]
          (if (terminate-pred next-ngram-node)
            (token-joiner token-node token-acc)
            (recur next-ngram-node (token-joiner token-node token-acc))))))))

(defn- token-node->ngram-nodes
  [token-node conn]
  (first (:data (cy/query conn "MATCH (token:Token)-[:in*1..1]->(ngram:Ngram) WHERE id(token) = {id} RETURN ngram" {:id (:id token-node)}))))

(defn- ngram-token-nodes->ngram-node
  [ngram-tokens conn]
  (first (first (:data (cy/query conn "MATCH (ngram:Ngram {hash: {hash}}) RETURN ngram" {:hash (hash ngram-tokens)})))))

(defn- build-backwards
  [ngram-node conn]
  (build-directional ngram-node ngram->prevs #(:start %) #(ngram-token-nodes->ngram-node (concat [(get-in %2 [:data :text])] (drop-last (get-in %1 [:data :tokens]))) conn) #(cons (get-in %1 [:data :text]) %2) conn))

(defn- build-forwards
  [ngram-node conn]
  (build-directional ngram-node ngram->nexts #(:end %) #(ngram-token-nodes->ngram-node (concat (rest (get-in %1 [:data :tokens])) [(get-in %2 [:data :text])]) conn) #(concat %2 [(get-in %1 [:data :text])]) conn))

(defn build-line
  [conn]
  (let [init-token-node (random-token conn)
        init-ngram-node (rand-nth (token-node->ngram-nodes init-token-node conn))]
    (string/join " " (concat (build-backwards init-ngram-node conn) (get-in init-ngram-node [:data :tokens]) (build-forwards init-ngram-node conn)))))
