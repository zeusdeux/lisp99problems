(ns lisp99problems.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Solutions to https://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html"))

(defn p01
  "Find the last box of a list"
  [xs]
  (loop [[y & ys] xs]
    (if (empty? ys)
      y
      (recur ys))))

(defn p02
  "Get the last two elements of a list"
  [xs]
  (loop [[y & ys :as all] xs]
    (if (<= (count all) 2)
      (into [] all)
      (recur ys))))

(defn p03
  "Find the K'th element of a list"
  [xs i]
  (do
    (if (< (count xs) i)
      (throw (Exception. "Index larger than length of seq"))
      (loop [[y & ys] xs
             curr-idx 0]
        (if (= i curr-idx)
          y
          (recur ys (inc curr-idx)))))))

(defn p04
  "Count of number of elements in list"
  [xs]
  (reduce (fn [acc _] (+ acc 1)) 0 xs))

(defn p05
  "Reverse list"
  [xs]
  (reduce #(into [%2] %1) []  xs))

(defn p06
  "Find out whether a list is a palindrome"
  [xs]
  (= xs (p05 xs)))

(defn p07
  "Flatten a nested list structure"
  [xs]
  (reduce (fn [acc x]
            (if (sequential? x)
              (into acc (p07 x))
              (conj acc x)))
          [] xs))

(defn p08
  "Eliminate consecutive duplicates of list elements"
  [xs]
  (reduce (fn [acc x]
            (if (= x (last acc))
              acc
              (conj acc x)))
          [] xs))

(defn p09
  "Pack consecutive duplicates of list elements into sublists"
  [xs]
  (reduce (fn [acc x]
            (let [l (last acc)]
              (if (= x (last l))
                (conj (pop acc) (conj l x))
                (conj acc [x]))))
          [] xs))

(defn p10
  "Run-length encoding of a list"
  [xs]
  (map #(vector (count %) (first %)) (p09 xs)))

(defn p11
  "Modify the result of problem P10 in such a way that if an
  element has no duplicates it is simply copied into the result
  list. Only elements with duplicates are transferred as (N E) lists."
  [xs]
  (map (fn
         [[n e :as pair]]
         (if (<= n 1)
           e
           pair))
       (p10 xs)))

(defn p12
  "Given a run-length code list generated as specified in problem P11.
  Construct its uncompressed version."
  [xs]
  (reduce #(into %1 %2) []  (map (fn [x]
                        (if (sequential? x)
                          (repeat (first x) (last x))
                          [x])) xs)))

(defn p13
  "Implement run-encoding in one go without using p09"
  [xs]
  (map (fn [[n e :as pair]]
         (if (<= n 1)
           e
           pair))
       (reduce (fn [acc x]
             (let [[n e] (last acc)]
               (if (= e x)
                 (conj (pop acc) [(+ 1 n) e])
                 (conj acc [1 x]))))
               [] xs)))

(defn p14
  "Duplicate the elements of a list."
  [xs]
  (loop [[y & ys :as all] xs
         res []]
    (if (empty? all)
      res
      (recur ys (into res [y y])))))

(defn p15
  "Replicate the elements of a list a given number of times"
  [xs n]
  (reduce (fn [acc x]
            (into acc (replicate n x)))
          [] xs))

(defn sum
  [xs]
  (loop [[y & ys :as all] xs
         res 0]
    (if (empty? all)
      res
      (recur ys (+ res y)))))
