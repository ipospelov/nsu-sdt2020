(ns labs.lab4.lab4
  (:require [clojure.test :as test]))

(defn variable [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable? [expr]
  (= (first expr) ::var))

(defn variable-name [vr]
  (second vr))

(defn same-variables? [vr1 vr2]
  {:pre [(and
           (variable? vr1)
           (variable? vr2))]}
  (=
    (variable-name vr1)
    (variable-name vr2)))


(defn constant [value]
  {:pre [(boolean? value)]}
  (list ::const value))

(defn constant? [expr]
  (= (first expr) ::const))

(defn constant-value [const]
  (second const))


(defn conjunction [expr & rest]
  (cons ::conj (cons expr rest)))

(defn conjunction? [expr]
  (= (first expr) ::conj))


(defn disjunction [expr & rest]
  (cons ::disj (cons expr rest)))

(defn disjunction? [expr]
  (= (first expr) ::disj))


(defn negation [expr & rest]
  (cons ::neg (cons expr rest)))

(defn negation? [expr]
  (= (first expr) ::neg))


(defn implication [expr & rest]
  (cons ::impl (cons expr rest)))

(defn implication? [expr]
  (= (first expr) ::impl))


; Utils
(defn atom? [expr]
  (or
    (variable? expr)
    (constant? expr)))

(defn args [expr]
  (rest expr))

(defn opposite? [a b]
  (if (negation? a)
    (= a (negation b))
    (= (negation a) b)))

(defn operation? [expr]
  ((some-fn conjunction?
            disjunction?
            negation?)
   expr))

(defn apply-rules [expr table & args]
  (let [transform (some
                    (fn [rule]
                      (if ((first rule) expr)
                        (second rule)
                        false))
                    table)]
    (if transform
      (transform expr args))))

; Step 1: Избавиться от всех логических операций,
; содержащихся в формуле, заменив их основными:
; конъюнкцией, дизъюнкцией, отрицанием.

(declare apply-to-basis-rules)

(def to-basis-rules
  (list
    [(fn [expr] (atom? expr))
     (fn [expr _] expr)]
    [(fn [expr] (conjunction? expr))
     (fn [expr _] (conjunction
                    (apply-to-basis-rules (first (args expr)))
                    (apply-to-basis-rules (second (args expr)))))]
    [(fn [expr] (disjunction? expr))
     (fn [expr _] (disjunction
                    (apply-to-basis-rules (first (args expr)))
                    (apply-to-basis-rules (second (args expr)))))]
    [(fn [expr] (negation? expr))
     (fn [expr _] (negation (apply-to-basis-rules (first (args expr)))))]
    [(fn [expr] (implication? expr))
     (fn [expr _] (disjunction
                    (negation (apply-to-basis-rules (first (args expr))))
                    (apply-to-basis-rules (second (args expr)))))]))

(defn apply-to-basis-rules [expr]
  (apply-rules expr to-basis-rules))

(test/deftest test
  (test/testing "Test basis rules"
    (test/is (= (disjunction? (apply-to-basis-rules (implication (variable :x) (variable :y)))) true))))
(test/run-tests 'labs.lab4.lab4)

; Step 2: Заменить знак отрицания, относящийся ко всему выражению,
; знаками отрицания, относящимися к отдельным переменным высказываниям
; !(A v B) === !A ^ !B
; !(A ^ B) === !A v !B
; !!A === A

(declare apply-exclude-negation-rules)

(def exclude-negation-rules
  (list
    [(fn [expr] (atom? expr))
     (fn [expr [outer-negated]] (if outer-negated
                                  (negation expr)
                                  expr))]
    [(fn [expr] (conjunction? expr))
     (fn [expr [outer-negated]] (if outer-negated
                                  (disjunction
                                    (apply-exclude-negation-rules
                                      (first (args expr))
                                      true)
                                    (apply-exclude-negation-rules
                                      (second (args expr))
                                      true))
                                  (conjunction
                                    (apply-exclude-negation-rules
                                      (first (args expr))
                                      false)
                                    (apply-exclude-negation-rules
                                      (second (args expr))
                                      false))))]
    [(fn [expr] (disjunction? expr))
     (fn [expr [outer-negated]] (if outer-negated
                                  (conjunction
                                    (apply-exclude-negation-rules
                                      (first (args expr))
                                      true)
                                    (apply-exclude-negation-rules
                                      (second (args expr))
                                      true))
                                  (disjunction
                                    (apply-exclude-negation-rules
                                      (first (args expr))
                                      false)
                                    (apply-exclude-negation-rules
                                      (second (args expr))
                                      false))))]
    [(fn [expr] (negation? expr))
     (fn [expr [outer-negated]] (if outer-negated
                                  (apply-exclude-negation-rules
                                    (first (args expr))
                                    false)
                                  (apply-exclude-negation-rules
                                    (first (args expr))
                                    true)))]))

(defn apply-exclude-negation-rules [expr outer-negated]
  (apply-rules expr exclude-negation-rules outer-negated))

(defn apply-second-stage-rules [expr]
  (apply-exclude-negation-rules expr false))

(test/deftest test
  (test/testing "Test De Morgan rules, double negation rule"
    (test/is (=
               (apply-second-stage-rules
                 (negation
                   (disjunction
                     (variable :x)
                     (variable :y))))
               (conjunction (negation (variable :x)) (negation (variable :y)))))
    (test/is (=
               (apply-second-stage-rules
                 (negation
                   (conjunction
                     (variable :x)
                     (variable :y))))
               (disjunction (negation (variable :x)) (negation (variable :y)))))
    (test/is (=
               (apply-second-stage-rules (negation (negation (variable :x))))
               (variable :x)))
    (test/is (=
               (apply-second-stage-rules (negation (variable :x)))
               (negation (variable :x))))
    (test/is (=
               (apply-second-stage-rules (variable :x))
               (variable :x)))))
(test/run-tests 'labs.lab4.lab4)

; Step 3: Применить, если нужно,
; к операциям конъюнкции и дизъюнкции свойства
; дистрибутивности и формулы поглощения.
; A ^ (B v C) === (A ^ B) v (A ^ C)
; (A v B) ^ C === (A ^ C) v (B ^ C)

(declare apply-distribution-rules)

(def distribution-rules
  (list
    [(fn [expr] (atom? expr))
     (fn [expr _] expr)]
    [(fn [expr] (negation? expr))
     (fn [expr _] (negation (apply-distribution-rules (first (args expr)))))]

    [(fn [expr] (and
                  (conjunction? expr)
                  (disjunction? (first (args expr)))))
     (fn [expr _] (disjunction
                    (conjunction
                      (apply-distribution-rules (first (args (first (args expr)))))
                      (apply-distribution-rules (second (args expr))))
                    (conjunction
                      (apply-distribution-rules (second (args (first (args expr)))))
                      (apply-distribution-rules (second (args expr))))))]

    [(fn [expr] (and
                  (conjunction? expr)
                  (disjunction? (second (args expr)))))
     (fn [expr _] (disjunction
                    (conjunction
                      (apply-distribution-rules (first (args expr)))
                      (apply-distribution-rules (first (args (second (args expr))))))
                    (conjunction
                      (apply-distribution-rules (first (args expr)))
                      (apply-distribution-rules (second (args (second (args expr))))))))]

    [(fn [expr] (disjunction? expr))
     (fn [expr _] (disjunction
                    (apply-distribution-rules (first (args expr)))
                    (apply-distribution-rules (second (args expr)))))]
    [(fn [expr] (conjunction? expr))
     (fn [expr _] (conjunction
                    (apply-distribution-rules (first (args expr)))
                    (apply-distribution-rules (second (args expr)))))]))

(defn apply-distribution-rules [expr]
  (apply-rules expr distribution-rules))

(test/deftest test
  (test/testing "Test distribution rules"
    (test/is (=
               (apply-distribution-rules
                 (conjunction
                   (variable :x)
                   (disjunction
                     (variable :y)
                     (variable :z))))
               (disjunction
                 (conjunction (variable :x) (variable :y))
                 (conjunction (variable :x) (variable :z)))))
    (test/is (=
               (apply-distribution-rules
                 (conjunction
                   (disjunction
                     (variable :x)
                     (variable :y))
                   (variable :z)))
               (disjunction
                 (conjunction (variable :x) (variable :z))
                 (conjunction (variable :y) (variable :z)))))))
(test/run-tests 'labs.lab4.lab4)

(declare apply-signification-rules)

(def signification-rules
  (list
    [(fn [expr] (constant? expr))
     (fn [expr _] expr)]

    [(fn [expr] (variable? expr))
     (fn [expr [variable value]]
       (if
         (same-variables? expr variable)
         (constant value)
         expr))]

    [(fn [expr] (conjunction? expr))
     (fn [expr [variable value]]
       (conjunction
         (apply-signification-rules
           (first (args expr))
           variable
           value)
         (apply-signification-rules
           (second (args expr))
           variable
           value)))]
    [(fn [expr] (disjunction? expr))
     (fn [expr [variable value]]
       (disjunction
         (apply-signification-rules
           (first (args expr))
           variable
           value)
         (apply-signification-rules
           (second (args expr))
           variable
           value)))]
    [(fn [expr] (negation? expr))
     (fn [expr [variable value]]
       (negation
         (apply-signification-rules
           (first (args expr))
           variable
           value)))]))

(defn apply-signification-rules [expr variable value]
  (apply-rules expr signification-rules variable value))

(test/deftest test
  (test/testing "Test signification"
    (test/is (=
               (apply-signification-rules (constant true) (variable :x) true)
               (constant true)))
    (test/is (=
               (apply-signification-rules
                 (disjunction
                   (variable :x)
                   (negation (variable :y)))
                 (variable :x)
                 true)
               (disjunction
                 (constant true)
                 (negation (variable :y)))))))
(test/run-tests 'labs.lab4.lab4)

(declare apply-simplification-rules)

(defn operation [expr]
  (when (operation? expr)
    (cond
      (conjunction? expr) conjunction
      (disjunction? expr) disjunction
      (negation? expr) negation)))

(def simplification-rules
  (list
    [(fn [expr] (and
                  (conjunction? expr)
                  (= (first (args expr)) (second (args expr)))))
     (fn [expr _] (apply-simplification-rules (first (args expr))))]

    [(fn [expr] (and
                  (disjunction expr)
                  (= (first (args expr)) (second (args expr)))))
     (fn [expr _] (apply-simplification-rules (first (args expr))))]

    [(fn [expr] (and
                  (conjunction? expr)
                  (= (first (args expr)) (constant false))))
     (fn [_ _] (constant false))]
    [(fn [expr] (and
                  (conjunction? expr)
                  (= (second (args expr)) (constant false))))
     (fn [_ _] (constant false))]

    [(fn [expr] (and (conjunction? expr)
                     (= (first (args expr)) (constant true))))
     (fn [expr _] (apply-simplification-rules (second (args expr))))]
    [(fn [expr] (and (conjunction? expr)
                     (= (second (args expr)) (constant true))))
     (fn [expr _] (apply-simplification-rules (first (args expr))))]

    [(fn [expr] (and (disjunction? expr)
                     (= (first (args expr)) (constant false))))
     (fn [expr _] (apply-simplification-rules (second (args expr))))]
    [(fn [expr] (and (disjunction? expr)
                     (= (second (args expr)) (constant false))))
     (fn [expr _] (apply-simplification-rules (first (args expr))))]

    [(fn [expr] (and (disjunction? expr)
                     (= (first (args expr)) (constant true))))
     (fn [_ _] (constant true))]
    [(fn [expr] (and (disjunction? expr)
                     (= (second (args expr)) (constant true))))
     (fn [_ _] (constant true))]

    [(fn [expr] (and
                  (conjunction? expr)
                  (let [[a b] (args expr)]
                    (opposite? a b))))
     (fn [_ _] (constant false))]

    [(fn [expr] (and
                  (disjunction? expr)
                  (let [[a b] (args expr)]
                    (opposite? a b))))
     (fn [_ _] (constant true))]

    [(fn [expr] (atom? expr))
     (fn [expr _] expr)]

    [(fn [expr] (operation? expr))
     (fn [expr _] (let [log-op (operation expr)
                        args (args expr)]
                    (apply log-op (map apply-simplification-rules args))))]
    ))

(defn apply-simplification-rules [expr]
  (apply-rules expr simplification-rules))
;
(test/deftest test
  (test/testing "Test simplification"
    (test/is (=
               (apply-simplification-rules (disjunction (variable :x) (negation (variable :x))))
               (constant true)))
    (test/is (=
               (apply-simplification-rules (disjunction (negation (variable :x)) (variable :x)))
               (constant true)))
    (test/is (=
               (apply-simplification-rules (conjunction (variable :x) (negation (variable :x))))
               (constant false)))
    (test/is (=
               (apply-simplification-rules (conjunction (negation (variable :x)) (variable :x)))
               (constant false)))
    (test/is (=
               (apply-simplification-rules (disjunction (variable :x) (constant true)))
               (constant true)))
    (test/is (=
               (apply-simplification-rules (disjunction (constant true) (variable :x)))
               (constant true)))
    (test/is (=
               (apply-simplification-rules (disjunction (variable :x) (constant false)))
               (variable :x)))
    (test/is (=
               (apply-simplification-rules (disjunction (constant false) (variable :x)))
               (variable :x)))
    (test/is (=
               (apply-simplification-rules (conjunction (variable :x) (constant true)))
               (variable :x)))
    (test/is (=
               (apply-simplification-rules (conjunction (constant true) (variable :x)))
               (variable :x)))
    (test/is (=
               (apply-simplification-rules (conjunction (variable :x) (constant false)))
               (constant false)))
    (test/is (=
               (apply-simplification-rules (conjunction (constant false) (variable :x)))
               (constant false)))
    (test/is (=
               (apply-simplification-rules (disjunction (variable :x) (variable :x)))
               (variable :x)))
    (test/is (=
               (apply-simplification-rules (conjunction (variable :x) (variable :x)))
               (variable :x)))))
(test/run-tests 'labs.lab4.lab4)

(defn make-dnf
  [expr]
  (->>
    expr
    (apply-to-basis-rules)
    (apply-second-stage-rules)
    (apply-distribution-rules)
    (apply-simplification-rules)
    ))

(test/testing "Test making DNF"
  (test/is (= (make-dnf (disjunction
                          (implication
                            (variable :x)
                            (variable :y))
                          (implication
                            (variable :y)
                            (variable :z))))
              (disjunction
                (disjunction
                  (negation (variable :x))
                  (variable :y))
                (disjunction
                  (negation (variable :y))
                  (variable :z))))))
(test/run-tests 'labs.lab4.lab4)
