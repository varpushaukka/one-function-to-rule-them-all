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
  [:-])

(defn min-max-element [a-seq]
  [:-])

(defn insert [sorted-seq n]
  [:-])

(defn insertion-sort [a-seq]
  [:-])

(defn parity [a-seq]
  [:-])

(defn minus [x]
  :-)

(defn count-params [x]
  :-)

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])