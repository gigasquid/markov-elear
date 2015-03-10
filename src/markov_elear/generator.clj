(ns markov-elear.core
  (:require [clojure.java.io :as io]))

(comment 
  (def example-2 "And the Golden Grouse came there, And the Pobble who has no toes")

  (def words (clojure.string/split example #" "))

  (def word-transitions (partition-all 3 1 words))

  (merge-with concat {:a [1]} {:a [3]})
  (merge-with clojure.set/union {:a #{1}} {:a #{2}})
  (clojure.set/union  #{1} #{2})


  (map (fn [t] (let [[a b c] t]
                {[a b] (if c [c] [])}))
       word-transitions))

(defn word-chain [word-transitions]
  (reduce (fn [r t] (merge-with clojure.set/union r
                               (let [[a b c] t]
                                 {[a b] (if c #{c} #{})})))
          {}
          word-transitions))

(defn walk-chain [prefix chain result]
  (let [suffixes (get chain prefix)]
    (if (or (> (count (apply str result)) 100) (empty? suffixes))
      result
      (let [n (rand-int (count suffixes))
            suffix (nth (seq suffixes) n)
            new-prefix [(last prefix) suffix]]
        (recur new-prefix chain (conj result suffix))))))


(defn text->word-chain [s]
  (let [words (clojure.string/split s #"[\s|\n]")
        word-transitions (partition-all 3 1 words)]
    (word-chain word-transitions)))

(defn generate-text [start-phrase word-chain]
  (let [prefix (clojure.string/split start-phrase #" ")]
    (apply str (interpose " "
                          (walk-chain prefix word-chain prefix)))))

(defn generate-text
  [start-phrase word-chain]
  (let [prefix (clojure.string/split start-phrase #" ")]
    (apply str (interpose " "
                          (walk-chain prefix word-chain prefix)))))


(comment 
  (generate-text "I am" (text->word-chain example))

  (generate-text "And the" (text->word-chain example-2))

  (generate-text "On the" (text->word-chain quangle-1))
  (generate-text  (text->word-chain quangle-1))

  (clojure.string/split "abcdefg" #"[a|d]")

  (slurp "resources/quangle-wangle.txt")
  (slurp "./resources/quangle-wangle.txt")
  (slurp (io/resource "blubber.txt")))

(defn process-file [fname]
  (text->word-chain
   (slurp (io/resource fname))))

(def files ["quangle-wangle.txt" "monad.txt" "clojure.txt" "functional.txt"
            "jumblies.txt" "pelican.txt" "pobble.txt"])
(def functional-leary (apply merge-with clojure.set/union (map process-file files)))

(def prefix-list ["On the" "They went" "And all" "We think"
                  "For every" "No other" "To a" "And every"
                  "We, too," "For his" "And the" "But the"
                  "Are the" "The Pobble" "For the" "When we"])

(defn tweet-text []
  (generate-text (-> prefix-list shuffle first) functional-leary))

(tweet-text)

(for [i (range 5)]
  (tweet-text))




