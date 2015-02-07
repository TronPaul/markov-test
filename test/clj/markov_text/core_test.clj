(ns markov-text.core-test
  (:require [clojure.test :refer :all]
            [markov-text.core :as core]))

(deftest ngram-tokens-test
  (testing "Create ngrams from tokens list"
    (is (= (seq [{:prev nil :ngram (seq ["The" "quick" "brown"]) :next "fox"}
                 {:prev "The" :ngram (seq ["quick" "brown" "fox"]) :next "jumped"}
                 {:prev "quick" :ngram (seq ["brown" "fox" "jumped"]) :next "over"}
                 {:prev "brown" :ngram (seq ["fox" "jumped" "over"]) :next "the"}
                 {:prev "fox" :ngram (seq ["jumped" "over" "the"]) :next "lazy"}
                 {:prev "jumped" :ngram (seq ["over" "the" "lazy"]) :next "dog."}
                 {:prev "over" :ngram (seq ["the" "lazy" "dog."]) :next nil}])
           (#'core/ngram-tokens (seq ["The" "quick" "brown" "fox" "jumped" "over" "the" "lazy" "dog."]) 3)))))