(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [helper (fn [acc]
                   (if (== (count acc) 1)
                     (first acc)
                     (recur (rest acc))))]
      (helper a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc n]
                 (if (>= n 0)
                   (if (== (nth seq1 n) (nth seq2 n))
                     (recur acc (dec n))
                     false)
                   acc))]
    (if (== (count seq1) (count seq2))
            (helper true (dec (count seq1)))
            false)))

(defn find-first-index [pred a-seq]
  (loop [seq1 a-seq
         acc 0]
    (if (empty? seq1)
      nil
      (if (pred (first seq1))
        acc
        (recur (rest seq1) (inc acc))))))

(defn avg [a-seq]
  (loop [sum1 0
         count1 0
         seq1 a-seq]
    (if (empty? seq1)
      (/ sum1 count1)
      (recur (+ sum1 (first seq1)) (inc count1) (rest seq1)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
    (loop [seq1 a-seq
           acc #{}]
      (if (empty? seq1)
        acc
        (recur (rest seq1) (toggle acc (first seq1)))))))

(defn fast-fibo [n]
  (loop [fn1-1 0
         fn1 1
         counter (dec n)]
    (if (< n 2)
      n
      (if (= counter 0)
        fn1
        (recur fn1 (+ fn1-1 fn1) (dec counter))))))

(defn cut-at-repetition [a-seq]
  (loop [seq1 a-seq
         repeats #{}
         results []]
    (if (empty? seq1)
      results
      (if (contains? repeats (first seq1))
        results
        (recur (rest seq1) (conj repeats (first seq1)) (conj results (first seq1)))))))
