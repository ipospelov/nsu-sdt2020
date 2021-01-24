(ns lab3.lab3-2
  (:require [clojure.test :as test]))

(defn separate
  [batch-size coll]
  (lazy-seq
    (when (seq coll)
      (cons (take batch-size coll)
            (separate
              batch-size
              (drop batch-size coll))))))


(defn filter-batch
  [pred batch]
  (->>
    batch
    (map #(future (doall (filter pred %))))
    (doall)
    (mapcat deref)))


(defn parallel-filter
  [pred n-threads batch-size coll]
  (->>
    (separate batch-size coll)
    (separate n-threads)
    (map #(filter-batch pred %))
    (apply concat)))


(defn heavy-pred
  [pred]
  (Thread/sleep 1000)
  pred)


(time (doall (take 5 (parallel-filter (heavy-pred odd?) 3 2 (range)))))
(time (doall (take 5 (filter (heavy-pred odd?) (range)))))

(test/deftest test
  (test/testing "Test separate"
    (test/is (= (separate 2 '(1 2 3)) '((1 2) (3))))
    (test/is (= (separate 2 '(1 2 3 4)) '((1 2) (3 4))))))

(test/deftest test
  (test/testing "Test parallel-filter"
    (test/is (= (take 5 (parallel-filter (heavy-pred odd?) 3 2 (range))) '(1 3 5 7 9)))))

(test/run-tests 'lab3.lab3-2)
