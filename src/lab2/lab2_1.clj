(ns lab2.lab2-1
  (:require [clojure.test :as test]))


(defn integrate
  ([fun x]
   (integrate fun x 0.01))
  ([fun x step-size]
   (* step-size (+ (/ (+ (fun 0) (fun x)) 2)
                   (reduce #(+ %1 (fun %2))
                           0
                           (drop-last (take (/ x step-size) (iterate #(+ % step-size) step-size))))))))

(def memoized-integrate (memoize integrate))

(time (memoized-integrate (fn [x] (* x x)) 10 0.001))
(time (memoized-integrate (fn [x] (* x x)) 10 0.001))

(test/deftest test
  (test/testing "Testing lab2_1"
    (test/is (= (integrate (fn [x] x) 10 1) 50))
    (test/is (= (integrate (fn [x] (* x x)) 5 1) 335))))

(test/run-tests 'labs.lab2.lab2-1)
