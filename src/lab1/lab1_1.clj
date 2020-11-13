(ns lab1.lab1-1)

(defn combine
  [result alphabet]
  (if (= (count alphabet) 0)
    (drop-last result)
    (if (= (first (last result)) (first alphabet))
      (combine result
               (rest alphabet))
      (combine (conj result
                     (conj (last result)
                           (first alphabet)))
               (rest alphabet)))))

(defn permute
  [coll alphabet acc]
  (if (= (count coll) 0)
    acc
    (permute (next coll)
             alphabet
             (concat acc (combine (list (first coll))
                                  alphabet)))))

(defn get-permutations
  ([alphabet n]
   (if (> n 1)
     (get-permutations alphabet n '(()))
     (if (= n 1)
       alphabet
       '())))
  ([alphabet n acc]
   (if (= n 0)
     acc
     (get-permutations alphabet
                       (- n 1)
                       (permute acc alphabet '())))))

(println (get-permutations '(:a "b" [1] 3/2) 2))
