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

(defn uniq-rows?
  "Return true if every queen is in unique row and false otherwise."
  [queens]
  (->> queens (map second) (into #{}) count (= (count queens))))

(defn diagonal
  "Return seq of positions [x y] iterating positions with iterate-fn starting at xy"
  [xy size iterate-fn]
  (->> (iterate iterate-fn xy)
       (take-while #(in-board? % size))))

(defn diagonals
  "Return set of all diagonal (ne/se/sw/nw) coords for position xy including xy itself."
  [xy size]
  (->> (concat (diagonal xy size #(mapv inc %))
               (diagonal xy size #(mapv dec %))
               (diagonal xy size (fn [[x y]] (vector (dec x) (inc y))))
               (diagonal xy size (fn [[x y]] (vector (inc x) (dec y)))))
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

(defn -main []
  (println "(qcheck [2 4 6 1 3 5]) =>" (qcheck [2 4 6 1 3 5]))
  (println "(qcheck [4 2 7 3 6 8 5 1]) =>" (qcheck [4 2 7 3 6 8 5 1]))
  (println "(qcheck [2 5 7 4 1 8 6 3]) =>" (qcheck [2 5 7 4 1 8 6 3]))
  (println "(qcheck [5 3 1 4 2 8 6 3]) =>" (qcheck [5 3 1 4 2 8 6 3]))
  (println "(qcheck [5 8 2 4 7 1 3 6]) =>" (qcheck [5 8 2 4 7 1 3 6]))
  (println "(qcheck [4 3 1 8 1 3 5 2]) =>" (qcheck [4 3 1 8 1 3 5 2])))

(t/deftest check-test
  (t/testing "qcheck"
    (t/is (true? (qcheck [2 4 6 1 3 5])))
    (t/is (true? (qcheck [4 2 7 3 6 8 5 1])))
    (t/is (true? (qcheck [2 5 7 4 1 8 6 3])))
    (t/is (false? (qcheck [5 3 1 4 2 8 6 3])))
    (t/is (false? (qcheck [5 8 2 4 7 1 3 6])))
    (t/is (false? (qcheck [4 3 1 8 1 3 5 2])))))
