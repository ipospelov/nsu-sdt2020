(ns lab2.lab2-2
  (:require [clojure.test :as test]))


(defn delta
  [fun x step-size]
  (* (/ (+ (fun x) (fun (+ x step-size))) 2) step-size))

(defn integral-sequence
  [fun x step-size]
  (reductions (fn [res i] (+ res (delta fun i step-size)))
              (range 0 (+ x step-size) step-size)))

(defn integrate
  [fun x step-size]
  (nth (integral-sequence fun x step-size) (- (/ x step-size) 1)))

(test/deftest test
  (test/testing "Testing lab2_2"
    (test/is (= (integrate (fn [x] x) 10 0.1) 99/2))
    (test/is (= (integrate (fn [x] (* x x)) 4 1) 43/2))))

(test/run-tests 'lab2.lab2-2)
