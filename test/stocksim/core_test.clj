(ns stocksim.core-test
  (:require [clojure.test :refer :all]
            [stocksim.core :refer :all]
            [stocksim.data :refer :all]
            [stocksim.genetic :refer :all]
            ))


(deftest data-test
  (testing "Can retrieve data"
    (is (seq (get-table "^GSPC")))
    (is (seq (series "^GSPC")))))

(defn mutator [c] 
  (if c
    (let [new-val ((if (> (rand) 0.5) + -)
                   (rand-int 5) (int c))]
      (char (cond 
              (> new-val 126) 126
              (< new-val 32) 32
              :else new-val)))
    (char (rand-in-range 32 126))))

(defn fitness [value target]
  (reduce 
   (fn [rank [a b]] (if (= a b) rank (dec rank)))
   0 (map vector value target)))

(deftest evolve-string
  (is
   (= "Hello World!"
      (->> (evolve 1000 0.01 mutator fitness "Hello World!")
           time
           first
           :value
           (apply str)))))
