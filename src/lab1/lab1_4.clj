(ns lab1.lab1-4)

(defn combine
  [result alphabet]
  (map #(conj result %) (filter #(not= % (first result)) alphabet)))

(defn permute
  [coll alphabet]
  (mapcat identity (map #(combine % alphabet) coll)))

(defn get-permutations
  [alphabet n]
  (reduce (fn [acc el] (permute acc alphabet))
          '(())
          (range n)))

(println (get-permutations '(:a "b" [1] 3/2) 2))
