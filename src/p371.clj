(ns ^{:doc "N queens validator
https://www.reddit.com/r/dailyprogrammer/comments/ab9mn7/20181231_challenge_371_easy_n_queens_validator"}
    p371
  (:require [clojure.set :as set]
            [clojure.test :as t]))

(defn enumerate [rows]
  (->> (map dec rows)
       (interleave (range 0 (count rows)))
       (partition 2)
       (map vec)
       vec))

(defn in-board? [[x y] size]
  (and (>= x 0) (>= y 0) (< x size) (< y size)))

(defn ne-xys [xy size]
  (->> (iterate #(mapv inc %) xy)
       (take-while #(in-board? % size))))

(defn sw-xys [xy size]
  (->> (iterate #(mapv dec %) xy)
       (take-while #(in-board? % size))))

(defn nw-xys [xy size]
  (->> (iterate (fn [[x y]] (vector (dec x) (inc y))) xy)
       (take-while #(in-board? % size))))

(defn se-xys [xy size]
  (->> (iterate (fn [[x y]] (vector (inc x) (dec y))) xy)
       (take-while #(in-board? % size))))

(defn uniq-rows?
  "Returns true if every queen is in unique row and false otherwise."
  [queens]
  (->> queens (map second) (into #{}) count (= (count queens))))

(defn diagonals [xy size]
  (->> (concat (ne-xys xy size)
               (sw-xys xy size)
               (nw-xys xy size)
               (se-xys xy size))
       (into #{})))

(defn uniq-diagonals?
  "Returns true if every queen is in unique diagonal and false otherwise."
  [queen-xys]
  (let [size (count queen-xys)]
    (->> (for [queen-xy queen-xys
               :let [diags (diagonals queen-xy size)]]
           (count (set/intersection diags queen-xys)))
         (reduce +)
         (= size))))

(defn qcheck [queen-rows]
  (let [queen-xys (into #{} (enumerate queen-rows))]
    (and (uniq-rows? queen-xys)
         (uniq-diagonals? queen-xys))))

(t/deftest check-test
  (t/testing "qcheck"
    (t/is (true? (qcheck [2 4 6 1 3 5])))
    (t/is (true? (qcheck [4 2 7 3 6 8 5 1])))
    (t/is (true? (qcheck [2 5 7 4 1 8 6 3])))
    (t/is (false? (qcheck [5 3 1 4 2 8 6 3])))
    (t/is (false? (qcheck [5 8 2 4 7 1 3 6])))
    (t/is (false? (qcheck [4 3 1 8 1 3 5 2])))))
