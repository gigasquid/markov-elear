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


(deftest test-walk-chain
  (let [chain {["the" "Pobble"] #{"who"}
               ["the" "Golden"] #{"Grouse"}
               ["And" "the"] #{"Pobble" "Golden"}}]
    (testing "dead end"
      (let [prefix ["the" "Pobble"]]
        (is (= ["the" "Pobble" "who"]
               (walk-chain prefix chain prefix)))))

    (testing "multiple choices"
      (with-redefs [rand-int (constantly 0)]
        (let [prefix ["And" "the"]]
          (is (= ["And" "the" "Pobble" "who"]
                 (walk-chain prefix chain prefix))))))))

(deftest test-text->word-chain
  (let [example "And the Golden Grouse And the Pobble who"]
    (is ( = {["who" nil] #{}
             ["Pobble" "who"] #{}
             ["the" "Pobble"] #{"who"}
             ["Grouse" "And"] #{"the"}
             ["Golden" "Grouse"] #{"And"}
             ["the" "Golden"] #{"Grouse"}
             ["And" "the"] #{"Pobble" "Golden"}}
            (text->word-chain example)))))

(deftest test-generate-text
  (with-redefs [rand-int (constantly 0)]
    (let [chain {["who" nil] #{}
                 ["Pobble" "who"] #{}
                 ["the" "Pobble"] #{"who"}
                 ["Grouse" "And"] #{"the"}
                 ["Golden" "Grouse"] #{"And"}
                 ["the" "Golden"] #{"Grouse"}
                 ["And" "the"] #{"Pobble" "Golden"}}]
      (is (= "the Pobble who" (generate-text "the Pobble" chain)))
      (is (= "And the Pobble who" (generate-text "And the" chain))))))
