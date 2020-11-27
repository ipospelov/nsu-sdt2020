(ns lab3.lab3-1
  (:require [clojure.test :as test]))

(defn separate
  [batch-size init-coll]
  (loop [acc '[] coll init-coll]
    (if (>= batch-size (count coll))
      (conj acc coll)
      (recur
        (conj acc (take batch-size coll))
        (drop batch-size coll)))))


(defn parallel-filter
  [pred n-threads coll]
  (->>
    (separate (int (Math/ceil (/ (count coll) n-threads))) coll)
    (map #(future (doall (filter pred %))))
    (doall)
    (mapcat deref)))


(defn heavy-pred
  [pred]
  (Thread/sleep 1000)
  pred)

(time (doall (parallel-filter (heavy-pred odd?) 2 '(1 2 3 4 5 6 7 8 9 10 11 12))))
(time (doall (filter (heavy-pred odd?) '(1 2 3 4 5 6 7 8 9 10 11 12))))

(test/deftest test
  (test/testing "Test separate"
    (test/is (= (separate 2 '(1 2 3)) '((1 2) (3))))
    (test/is (= (separate 2 '(1 2 3 4)) '((1 2) (3 4))))))

(test/deftest test
  (test/testing "Test parallel-filter"
    (test/is (= (parallel-filter (heavy-pred odd?) 3 '(1 2 3 4 5 6 7 8 9 10)) '(1 3 5 7 9)))))

(test/run-tests 'lab3.lab3-1)
