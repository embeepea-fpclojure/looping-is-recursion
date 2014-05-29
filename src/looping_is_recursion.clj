(ns looping-is-recursion)

(defn power [base exp]
  (let [power-helper (fn [acc base exp]
                       (if (<= exp 0)
                         acc
                         (recur (* acc base) base (dec exp))))]
    (power-helper 1 base exp)))

(defn last-element [a-seq]
  (let [last-element-helper (fn [acc a-seq]
                              (if (empty? a-seq)
                                acc
                                (recur (first a-seq) (rest a-seq))))]
    (last-element-helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [e1 e2 seq1 seq2]
                 (cond
                  (not (= e1 e2))                       false  ; first elements differ
                  (and (empty? seq1) (empty? seq2))     true   ; both are empty
                  (not (= (empty? seq1) (empty? seq2))) false  ; just one is empty
                  :else (recur (first seq1) (first seq2) (rest seq1) (rest seq2))))]
    (helper (first seq1) (first seq2) (rest seq1) (rest seq2))))


(defn find-first-index [pred a-seq]
  (loop [i 0,
         s a-seq]
    (cond
     (empty? s)       nil
     (pred (first s)) i
     :else (recur (inc i) (rest s)))))

(defn avg [a-seq]
  (loop [sum 0
         cnt 0
         s   a-seq]
    (if (empty? s)
      (/ sum cnt)
      (recur (+ sum (first s)) (inc cnt) (rest s)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
  (loop [st #{}
         sq a-seq]
    (if (empty? sq)
      st
      (recur (toggle st (first sq)) (rest sq))))))

(defn fast-fibo [n]
  (cond
   (<= n 0) 0
   (=  n 1) 1
   :else (loop [a 0,
                b 1
                i 2]
           (if (= i n) (+ a b)
               (recur b (+ a b) (inc i))))))

(defn cut-at-repetition [a-seq]
  (loop [st #{}
         v  []
         s  a-seq]
    (if (or
         (empty? s)
         (contains? st (first s)))
      v
      (recur (conj st (first s)) (conj v (first s)) (rest s)))))


