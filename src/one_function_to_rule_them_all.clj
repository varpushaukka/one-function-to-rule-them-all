(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce into [] a-seq)) 

(defn str-cat [a-seq]
  (if (empty? a-seq) ""
      (reduce (fn [a b] (str a " " b)) a-seq)))

(defn my-interpose [x a-seq]
  (let [interposed (fn [a b]
                     (conj a x b))]
    (rest (reduce interposed [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [a b] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [a b] (cons b a)) [] a-seq))

(defn min-max-element [a-seq]
  (let [minmax (fn [[min max] elem]
                 (cond 
                   (< elem min) [elem max]
                   (> elem max) [min elem]
                   :else [min max]
                   ))]
    (reduce minmax [(first a-seq) (first a-seq)] a-seq)))

(defn insert [sorted-seq n]
  (cond 
    (empty? sorted-seq) [n]
    (< (first sorted-seq) n) (cons (first sorted-seq) (insert (rest sorted-seq) n))
    :else (cons n sorted-seq)
    ))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [party (fn [set elem]
         (if (even? (count (filter #(= % elem) a-seq)))
           (disj set elem)
           set)
         )]
  (reduce party (set a-seq) a-seq)))

(defn minus 
  ([x] (* -1 x))
  ([x y] (- x y))
  )

(defn count-params [& x]
  (let [counter (fn [count elem]
                  (inc count))]
    (reduce counter 0 x)))

(defn my-* 
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more]
   (reduce * (* x y) more))
  )

(defn pred-and
  ([] (fn [x] true))
  ([x] (fn [x] x))
  ([p y] (fn [x] (and (p x) (y x))))
  ([p y & more]
   (fn [x] (reduce 
            (fn [val pred]
              (and (pred x) val)) (and (p x) (y x)) more))))

(defn my-map [f a-seq]
  [:-])