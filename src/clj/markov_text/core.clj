(ns markov-text.core
  (:require [clojurewerkz.neocons.rest :as nr]
            [clojurewerkz.neocons.rest.nodes :as nn]
            [clojurewerkz.neocons.rest.relationships :as nrl]
            [clojurewerkz.neocons.rest.constraints :as nc]
            [clojurewerkz.neocons.rest.labels :as nl]
            [clojurewerkz.neocons.rest.cypher :as cy]
            [clojurewerkz.neocons.rest.transaction :as tx]
            [clojure.string :as string]
            [roul.random :as rr])
  (:import (markov_text UnknownDirectionException)))

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
    (let [ngram-node (nn/create-unique-in-index conn "ngrams" "ngram" (hash ngram) {:hash (hash ngram) :tokens ngram :start start :end end :freq 0})]
      (if (and start (not (get-in ngram-node [:data :start])))
        (nn/set-property conn ngram-node :start true))
      (if (and end (not (get-in ngram-node [:data :end])))
        (nn/set-property conn ngram-node :end true))
      (nl/add conn ngram-node "Ngram")
      (nn/get conn (:id ngram-node)))))

(defn- store-links
  [ngram conn]
  (let [ngram-node (get-or-create-ngram (:ngram ngram) conn {:start (nil? (:prev ngram)) :end (nil? (:next ngram))})
        ngram-token-nodes (map #(get-or-create-token % conn) (:ngram ngram))]
    (nn/set-property conn ngram-node :freq (inc (get-in ngram-node [:data :freq] 0)))
    (if (:prev ngram)
      (let [prev-rel (nrl/maybe-create conn (get-or-create-token (:prev ngram) conn) ngram-node :chain {:freq 0})]
        (nrl/update conn prev-rel (merge (:data prev-rel) {:freq (inc (get-in prev-rel [:data :freq] 0))}))))
    (if (:next ngram)
      (let [next-rel (nrl/maybe-create conn ngram-node (get-or-create-token (:next ngram) conn) :chain {:freq 0})]
        (nrl/update conn next-rel (merge (:data next-rel) {:freq (inc (get-in next-rel [:data :freq] 0))}))))
    (doall (map #(nrl/maybe-create conn % ngram-node :in) ngram-token-nodes))))

(defn- store-chain
  [ngrams conn]
  (map #(store-links % conn) ngrams))

(defn format-tokens [tokens]
  (map string/lower-case tokens))

(defn add-line
  [line conn ngram-size]
  (let [line-tokens (format-tokens (tokenize line))
        ngrams (tokens->ngrams line-tokens ngram-size)]
    (doall (store-chain ngrams conn))))

(defn- random-token
  [conn]
  (rand-nth (nl/get-all-nodes conn "Token")))

(defn- ngram->next-tokens
  [ngram-node direction conn]
  (:data (cond
           (= :backward direction) (cy/query conn "MATCH (target:Ngram)<-[r:chain]-(prev:Token) WHERE id(target) = {id} RETURN r.freq, prev" {:id (get-in ngram-node [:metadata :id])})
           (= :forward direction) (cy/query conn "MATCH (target:Ngram)-[r:chain]->(next:Token) WHERE id(target) = {id} RETURN r.freq, next" {:id (get-in ngram-node [:metadata :id])})
           :else (throw (UnknownDirectionException. direction)))))

(defn- ngram-token-nodes->ngram-node
  [ngram-tokens conn]
  (first (first (:data (cy/query conn "MATCH (ngram:Ngram {hash: {hash}}) RETURN ngram" {:hash (hash ngram-tokens)})))))

(defn- ngram+token->next-ngram-tokens
  [ngram-node token-node direction]
  (cond
    (= :backward direction) (concat [(get-in token-node [:data :text])] (drop-last (get-in ngram-node [:data :tokens])))
    (= :forward direction) (concat (rest (get-in ngram-node [:data :tokens])) [(get-in token-node [:data :text])])
    :else (throw (UnknownDirectionException. direction))))

(defn- ngram+token->ngram-node
  [ngram-node token-node direction conn]
  (ngram-token-nodes->ngram-node (ngram+token->next-ngram-tokens ngram-node token-node direction) conn))

(defn- ngram+tokens->ngram-data-maps
  [ngram-node token-freq-node-pairs direction conn]
  (map (fn [[tf tn]]
         {:token-freq tf :token-node tn :ngram-node (ngram+token->ngram-node ngram-node tn direction conn)}) token-freq-node-pairs))

(defn- ngram-count
  [conn]
  (first (first (:data (cy/query conn "MATCH (n:Ngram) RETURN count(n)")))))

(defn- max-freq
  [ngram-node direction conn]
  (first (first (:data (cond
                         (= :backward direction) (cy/query conn "MATCH (n:Ngram)<-[r:chain]-(t:Token) WHERE id(n) = {id} RETURN count(r)" {:id (get-in ngram-node [:metadata :id])})
                         (= :forward direction) (cy/query conn "MATCH (n:Ngram)-[r:chain]->(t:Token) WHERE id(n) = {id} RETURN count(r)" {:id (get-in ngram-node [:metadata :id])})
                         :else (throw (UnknownDirectionException. direction)))))))

(defn- token-chain-count
  [token-node direction conn]
  (first (first (:data (cond
                         (= :backward direction) (cy/query conn "MATCH (t:Token)-[r:chain]->(n:Ngram) WHERE id(t) = {id} RETURN count(r)" {:id (get-in token-node [:metadata :id])})
                         (= :forward direction) (cy/query conn "MATCH (t:Token)<-[r:chain]-(n:Ngram) WHERE id(t) = {id} RETURN count(r)" {:id (get-in token-node [:metadata :id])})
                         :else (throw (UnknownDirectionException. direction)))))))

(defn- wegighted-tf
  [chain-freq max-chain-freq]
  (+ 0.5 (/ (* 0.5 chain-freq) max-chain-freq)))

(defn- idf
  [total-ngram-count token-chain-count]
  (Math/log (/ total-ngram-count (inc token-chain-count))))

(defn- tf-idf
  [chain-freq max-chain-freq total-ngram-count token-chain-count]
  (* (wegighted-tf chain-freq max-chain-freq) (idf total-ngram-count token-chain-count)))

(defn- weight-tf-idf
  [{:keys [ngram-node token-node token-freq]}  direction conn]
  (tf-idf token-freq (max-freq ngram-node direction conn) (ngram-count conn) (token-chain-count token-node direction conn)))

(defn- select-next-token-tf-idf
  [ngram-node token-freq-node-pairs direction conn]
  (rr/rand-nth-weighted (map (fn [m]
                               [m (weight-tf-idf m direction conn)]) (ngram+tokens->ngram-data-maps ngram-node token-freq-node-pairs direction conn))))

(defn- token-node->ngram-nodes
  [token-node conn]
  (map #(first %) (:data (cy/query conn "MATCH (token:Token)-[:in]->(ngram:Ngram) WHERE id(token) = {id} RETURN ngram" {:id (:id token-node)}))))

(defn- terminate-pred
  [ngram-node direction]
  (cond
    (= :backward direction) (:start ngram-node)
    (= :forward direction) (:end ngram-node)
    :else (throw (UnknownDirectionException. direction))))

(defn- token-joiner
  [token acc direction]
  (cond
    (= :backward direction) (cons (get-in token [:data :text]) acc)
    (= :forward direction) (concat acc [(get-in token [:data :text])])
    :else (throw (UnknownDirectionException. direction))))

(defn- build-directional
  [ngram-node direction conn]
  (loop [ngram-node ngram-node
         token-acc nil]
    (let [token-freq-node-pairs (ngram->next-tokens ngram-node direction conn)]
      (if (empty? token-freq-node-pairs)
        token-acc
        (let [{next-ngram-node :ngram-node :keys [token-node]} (select-next-token-tf-idf ngram-node token-freq-node-pairs direction conn)]
          (if (terminate-pred next-ngram-node direction)
            (token-joiner token-node token-acc direction)
            (recur next-ngram-node (token-joiner token-node token-acc direction))))))))

(defn build-line
  [conn]
  (let [init-token-node (random-token conn)
        init-ngram-node (rand-nth (token-node->ngram-nodes init-token-node conn))]
    (string/join " " (concat (build-directional init-ngram-node :backward conn) (get-in init-ngram-node [:data :tokens]) (build-directional init-ngram-node :forward conn)))))
