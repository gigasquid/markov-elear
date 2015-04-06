(ns markov-elear.generator-test
  (:require [clojure.test :refer :all]
            [markov-elear.generator :refer :all]))

(deftest test-word-chain
  (testing "it produces a chain of the possible two step transitions between suffixes and prefixes"
    (let [example '(("And" "the" "Golden")
                    ("the" "Golden" "Grouse")
                    ("And" "the" "Pobble")
                    ("the" "Pobble" "who"))]
      (is (= {["the" "Pobble"] #{"who"}
              ["the" "Golden"] #{"Grouse"}
              ["And" "the"] #{"Pobble" "Golden"}}
             (word-chain example))))))

(deftest test-text->word-chain
  (testing "string with spaces and newlines"
    (let [example "And the Golden Grouse\nAnd the Pobble who"]
     (is ( = {["who" nil] #{}
              ["Pobble" "who"] #{}
              ["the" "Pobble"] #{"who"}
              ["Grouse" "And"] #{"the"}
              ["Golden" "Grouse"] #{"And"}
              ["the" "Golden"] #{"Grouse"}
              ["And" "the"] #{"Pobble" "Golden"}}
             (text->word-chain example))))))

(deftest test-walk-chain
  (let [chain {["who" nil] #{},
               ["Pobble" "who"] #{},
               ["the" "Pobble"] #{"who"},
               ["Grouse" "And"] #{"the"},
               ["Golden" "Grouse"] #{"And"},
               ["the" "Golden"] #{"Grouse"},
               ["And" "the"] #{"Pobble" "Golden"}}]
    (testing "dead end"
      (let [prefix ["the" "Pobble"]]
        (is (= ["the" "Pobble" "who"]
               (walk-chain prefix chain prefix)))))
    (testing "multiple choices"
      (with-redefs [shuffle (fn [c] c)]
        (let [prefix ["And" "the"]]
          (is (= ["And" "the" "Pobble" "who"]
                 (walk-chain prefix chain prefix))))))
    (testing "repeating chains"
      (with-redefs [shuffle (fn [c] (reverse c))]
        (let [prefix ["And" "the"]]
          (is (> 140
                 (count (apply str (walk-chain prefix chain prefix)))))
          (is (= ["And" "the" "Golden" "Grouse" "And" "the" "Golden" "Grouse"]
                 (take 8 (walk-chain prefix chain prefix)))))))))

(deftest test-generate-text
  (with-redefs [shuffle (fn [c] c)]
    (let [chain {["who" nil] #{}
                 ["Pobble" "who"] #{}
                 ["the" "Pobble"] #{"who"}
                 ["Grouse" "And"] #{"the"}
                 ["Golden" "Grouse"] #{"And"}
                 ["the" "Golden"] #{"Grouse"}
                 ["And" "the"] #{"Pobble" "Golden"}}]
      (is (= "the Pobble who" (generate-text "the Pobble" chain)))
      (is (= "And the Pobble who" (generate-text "And the" chain))))))

(deftest test-end-at-last-puntcuation
  (testing "Ends at the last puncuation"
    (is (= "In a tree so happy are we."
           (end-at-last-punctuation "In a tree so happy are we. So that")))
    (testing "Replaces ending comma with a period"
    (is (= "In a tree so happy are we."
           (end-at-last-punctuation "In a tree so happy are we, So that"))))
    (testing "If there are no previous puncations, just leave it alone and add one at the end"
      (is ( = "In the light of the blue moon."
              (end-at-last-punctuation  "In the light of the blue moon there"))))
    (testing "works with multiple punctuation"
      (is ( = "In the light of the blue moon.  We danced merrily."
              (end-at-last-punctuation  "In the light of the blue moon.  We danced merrily.  Be"))))))
