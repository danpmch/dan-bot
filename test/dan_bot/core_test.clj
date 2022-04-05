(ns dan-bot.core-test
  (:require [clojure.test :refer :all]
            [dan-bot.core :refer :all]))

(deftest roll-dice-test
  (testing "dice"
    (are [max-val dice]
         (<= 1 (first (:values (roll-dice dice))) max-val)
      1 "1d1"
      6 "1d6"
      10 "1d10"))
  (testing "lead number optional"
    (are [max-val dice]
         (<= 1 (first (:values (roll-dice dice))) max-val)
      1 "d1"
      6 "d6"
      10 "d10"))
  (testing "multidice"
    (are [n]
         (= n (-> (str n "d6")
                  roll-dice
                  :values
                  count))
      1 2 3 4 5 6))
  (testing "modifiers"
    (are [total dice]
         (= total
            (:total (roll-dice dice)))
      2 "d1+1"
      0 "d1-1"
      -1 "d1-2"
      3 "2d1+1")))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))
