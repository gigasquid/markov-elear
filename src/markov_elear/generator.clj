(ns markov-elear.generator
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [overtone.at-at :as overtone]
            [twitter.api.restful :as twitter]
            [twitter.oauth :as twitter-oauth]
            [environ.core :refer [env]]))

(defn word-chain [word-transitions]
  (reduce (fn [r t] (merge-with set/union r
                               (let [[a b c] t]
                                 {[a b] (if c #{c} #{})})))
          {}
          word-transitions))

(defn walk-chain [prefix chain result]
  (let [suffixes (get chain prefix)]
    (if (or (> (count (apply str result)) 120) (empty? suffixes))
      result
      (let [n (rand-int (count suffixes))
            suffix (nth (seq suffixes) n)
            new-prefix [(last prefix) suffix]]
        (recur new-prefix chain (conj result suffix))))))

(defn text->word-chain [s]
  (let [words (clojure.string/split s #"[\s|\n]")
        word-transitions (partition-all 3 1 words)]
    (word-chain word-transitions)))

(defn end-at-last-punctuation [text]
  (let [trimmed-text (apply str (re-seq #"[\s\w]+[^.!?,]*[.!?,]" text))
        cleaned-text (clojure.string/replace trimmed-text #",$" ".")]
    (clojure.string/replace cleaned-text #"\"" "'")))

(defn generate-text
  [start-phrase word-chain]
  (let [prefix (clojure.string/split start-phrase #" ")
        result-chain (walk-chain prefix word-chain prefix)
        result-text (apply str (interpose " " result-chain))]
    result-text))

(defn process-file [fname]
  (text->word-chain
   (slurp (io/resource fname))))

(def files ["quangle-wangle.txt" "monad.txt" "clojure.txt" "functional.txt"
            "jumblies.txt" "pelican.txt" "pobble.txt"])
(def functional-leary (apply merge-with clojure.set/union (map process-file files)))

(def prefix-list ["On the" "They went" "And all" "We think"
                  "For every" "No other" "To a" "And every"
                  "We, too," "For his" "And the" "But the"
                  "Are the" "The Pobble" "For the" "When we"
                  "In the" "Yet we" "With only" "Are the"
                  "Though the" "To do" "And when" "As soon"
                  "We sit" "And this" "No other" "With a"
                  "None but" "And at" "What a" "Of the"
                  "O please" "So that" "And all" "When they"
                  "But before" "Whoso had" "And noboby" "And it's"])

(def my-pool (overtone/mk-pool))

(defn tweet-text []
  (let [text (generate-text (-> prefix-list shuffle first) functional-leary)]
    (end-at-last-punctuation text)))


(def my-creds (twitter-oauth/make-oauth-creds (env :app-consumer-key)
                                              (env :app-consumer-secret)
                                              (env :user-access-token)
                                              (env :user-access-secret)))

(defn status-update []
  (let [tweet (tweet-text)]
    (println "generated tweet is :" tweet)
    (when (not-empty tweet)
      (try (twitter/statuses-update :oauth-creds my-creds
                                    :params {:status tweet})
           (catch Exception e (println "Oh no! " (.getMessage e)))))))

(defn -main [& args]
  ;; every 8 hours
  (println "Started up")
  (println (tweet-text))
  (overtone/every (* 1000 60 60 8) #(println (status-update)) my-pool))

