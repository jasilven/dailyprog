(ns ^{:doc "https://www.reddit.com/r/dailyprogrammer/comments/cmd1hb/20190805_challenge_380_easy_smooshed_morse_code_1/"}
    ch-380
  (:require [clojure.string :as str]
            [clojure.test :as t]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(def letters (map char (range 97 (+ 26 97))))
(def morses (vec (str/split ".- -... -.-. -.. . ..-. --. .... .. .--- -.- .-.. -- -. --- .--. --.- .-. ... - ..- ...- .-- -..- -.-- --.." #" ")))
(def letter2morse (zipmap letters morses))
(def path "resources/input-380.txt")

(defn smorse
  "Returns concatenated morse code for s."
  [s]
  (->> (map #(get letter2morse %) s)
       (reduce str "")))

(defn update-codes [codes [morse word]]
  (assoc codes morse (conj (get codes morse) word)))

(defn parse-input
  "Returns map where key is concatenated morse code and value is seq of words for produces the morse code."
  [path]
  (with-open [rdr (io/reader path)]
    (reduce update-codes {} (for [word (line-seq rdr)]
                              [(smorse word) word ]))))

(defn bonus1 [codes cnt]
  (->> codes
       (filter #(= cnt (count (second %))))
       ffirst))

(defn bonus2 [codes s]
  (->> codes
       (filter #(str/includes? (first %) s))
       first
       second
       first))

(defn bonus3 [codes len]
  (->> codes
       (filter #(= (count (filter (fn [s] (= \- s)) (first %)))
                   (count (filter (fn [s] (= \. s)) (first %)))))
       (filter #(some (fn [word] (= len (.length word))) (second %)))
       first
       second
       first))

(defn palindrome? [s]
  (= s (str/reverse s)))

(defn bonus4 [codes len]
  (->> codes
       (filter #(palindrome? (first %)))
       (map second)
       (filter #(some (fn [word] (= len (.length word))) %))
       ffirst))

(defn bonus5 [codes known]
  (let [smorses (keys codes)]
    (->> (combo/selections [\. \-] (.length known))
         (map #(apply str %))
         (filter #(= 13 (.length %)))
         (remove #(some (fn [morse] (str/includes? morse %)) smorses))
         (remove #(= known %)))))

(defn solve []
  (let [codes (parse-input path)
        b1 (future (bonus1 codes 13))
        b2 (future (bonus2 codes "---------------"))
        b3 (future (bonus3 codes 21))
        b4 (future (bonus4 codes 13))
        b5 (future (bonus5 codes "--.---.---.--"))]
    (println "Bonus 1:" @b1)
    (println "Bonus 2:" @b2)
    (println "Bonus 3:" @b3)
    (println "Bonus 4:" @b4)
    (println "Bonus 5:" @b5)))

(defn -main []
  (time (solve))
  (shutdown-agents))

(t/deftest smorse-test
  (t/testing "morse enconding"
    (t/is (= "...---..." (smorse "sos")))
    (t/is (= "-...-...-..-.--" (smorse "daily")))
    (t/is (= ".--..-.-----..-..-----..-." (smorse "programmer")))
    (t/is (= "-.....-..." (smorse "bits")))
    (t/is (= "-.....-..." (smorse "three"))))
  (t/testing "bonus1"
    (t/is (= (sort '("needing" "nervate" "niding" "tiling"))
             (sort (get (parse-input path) "-...-....-.--."))))))
