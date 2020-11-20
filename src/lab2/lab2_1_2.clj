(ns lab2.lab2-1-2)

; possibly more suitable solution
(defn delta
  [fun x step-size]
  (* (/ (+ (fun x) (fun (+ x step-size))) 2) step-size))

(defn integrate
  [fun x step-size]
  (if (<= (- x step-size) 0)
    (delta fun x step-size)
    (+ (delta fun x step-size)
       (memoized-integrate fun
                           (- x step-size)
                           step-size))))

(def memoized-integrate (memoize integrate))
