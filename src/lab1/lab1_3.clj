(ns lab1.lab1-3)

(defn my-map
  [f coll]
  (reduce #(conj %1 (f %2)) [] coll))

(println (= (map #(* % 5) [1 2 3 4])
            (my-map #(* % 5) [1 2 3 4])))

(defn my-filter
  [pred coll]
  (reduce #(if (pred %2)
             (conj %1 %2)
             %1)
          []
          coll))

(println (= (my-filter odd? (range 10)) (filter odd? (range 10))))
