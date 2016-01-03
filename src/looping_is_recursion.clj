(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* base acc) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc]
                 (if (< (count acc) 2)
                   (first acc)
                   (recur (rest acc))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc1 acc2]
                 (cond
                   (and (empty? acc1) (empty? acc2)) true
                   (and (empty? acc1) (not (empty? acc2))) false
                   (and (not (empty? acc1)) (empty? acc2)) false
                   (= (first acc1) (first acc2)) (recur (rest acc1) (rest acc2))
                   :else false))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [acc a-seq
         n 0]
    (if (empty? acc)
      nil
      (if (pred (first acc))
        n
        (recur (rest acc) (inc n))))))

(defn avg [a-seq]
  (loop [acc 0
         n 0
         s a-seq]
    (if (empty? s)
      (if (= n 0)
        0
        (/ acc n))
      (recur (+ acc (first s)) (inc n) (rest s)))))

(defn parity [a-seq]
  (loop [a-set (set a-seq)
        frq (frequencies a-seq)]
    (if (empty? frq)
      a-set
      (if (even? (last (first frq)))
        (recur (disj a-set (first (first frq))) (rest frq))
        (recur a-set (rest frq))))))

(defn fast-fibo [n]
  (loop [a 0
         b 1
         c 1
         limit 0]
    (if (>= limit n)
      a
      (recur b c (+ b c) (inc limit)))))


(defn cut-at-repetition [a-seq]
  (loop [f (first a-seq)
         current (first (rest a-seq))
         i 1
         s (rest a-seq)]
    (if (or (empty? s) (= f current))
      (take i a-seq)
      (recur f (first (rest s)) (inc i) (rest s)))))
