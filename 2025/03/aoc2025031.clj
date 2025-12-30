(ns aoc2025031
  (:require [clojure.string :as str]))

(defn read-input []
  (let [file-name (nth *command-line-args* 0)]
    (str/trim-newline (slurp file-name))))

(defn parse-lines
  [input & [parse-fn nl-sep]]
  (mapv #(parse-fn %)
        (str/split input nl-sep)))

(defn max-joltage-from
  ([rating]
   (max-joltage-from rating 0))
  ([rating offset]
   (let [len (.length rating)]
     (->> rating
          seq
          (map-indexed #(vector %1 (Character/digit %2 10)))
          vec
          (#(subvec % offset (- len (if (zero? offset) 1 0))))
          reverse
          (apply max-key second)))))

(defn max-joltage [rating]
  (let [first-largest (max-joltage-from rating)
        first-offset (first first-largest)
        first-value (second first-largest)
        next-largest (max-joltage-from rating (inc first-offset))
        next-value (second next-largest)]
    (->>
     (str first-value next-value)
     parse-long)))

(defn solve [input]
  (let [spec (parse-lines input identity #"\n")]
    (reduce + (map max-joltage spec))))

(println (str "Joltage: " (solve (read-input))))
