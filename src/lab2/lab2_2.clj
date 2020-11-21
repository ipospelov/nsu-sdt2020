(ns lab2.lab2-2
  (:require [clojure.test :as test]))


(defn delta
  [fun x step-size]
  (* (/ (+ (fun x) (fun (+ x step-size))) 2) step-size))

(defn integral-sequence
  [fun step-size]
  (reductions (fn [res i] (+ res (delta fun i step-size)))
              (map #(* step-size %) (range))))

(defn get-integrator
  [fun step-size]
  (let [seq (integral-sequence fun step-size)]
    (fn [x] (nth seq (- (/ x step-size) 1)))))

(let [integrate-f (get-integrator (fn [x] x) 0.01)]
  (println (time (integrate-f 1000)))
  (println (time (integrate-f 1010)))
  (println (time (integrate-f 2000))))

(test/deftest test
  (test/testing "Testing lab2_2"
    (let [integrate-f (get-integrator (fn [x] x) 1)]
      (test/is (= (integrate-f 10) 99/2))
      (test/is (= (integrate-f 100) 9999/2)))))

(test/run-tests 'lab2.lab2-2)
