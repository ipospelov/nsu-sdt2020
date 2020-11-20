(ns lab1.lab1-2)

(defn combine
  [base alphabet acc]
  (if (= (count alphabet) 0)
    acc
    (if (= (first base) (first alphabet))
      (recur base
             (rest alphabet)
             acc)
      (recur base
             (rest alphabet)
             (conj acc
                   (conj base
                         (first alphabet)))))))

(defn permute
  [coll alphabet acc]
  (if (= coll nil)
    acc
    (recur (next coll)
           alphabet
           (concat acc (combine (first coll)
                                alphabet
                                '())))))

(defn get-permutations-internal
  [alphabet n acc]
  (if (= n 0)
    acc
    (recur alphabet
           (- n 1)
           (permute acc alphabet '()))))


(defn get-permutations
  [alphabet n]
  (if (> n 1)
    (get-permutations-internal alphabet n '(()))
    (if (= n 1)
      alphabet
      '())))

(println (get-permutations '(:a "b" [1] 3/2) 2))
