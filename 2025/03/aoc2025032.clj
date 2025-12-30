(ns aoc2025032
  (:require [clojure.string :as str]))

; number of banks that can be turned on
(defn banks [] 12)

(defn read-input []
  (let [file-name (nth *command-line-args* 0)]
    (str/trim-newline (slurp file-name))))

(defn parse-lines
  [input & [parse-fn nl-sep]]
  (mapv #(parse-fn %)
        (str/split input nl-sep)))

(defn max-joltage-from
  [rating banks count offset]
  (let [len (.length rating)
        min-chars-from-end (+ (- len banks) count)
        offset-from-end (-> len (- banks) inc (+ offset))]
    (->> rating
         seq
         (map-indexed #(vector %1 (Character/digit %2 10)))
         vec
         (#(subvec % offset (min len min-chars-from-end offset-from-end)))
         reverse
         (apply max-key second))))

(defn max-joltage
  ([rating] (max-joltage rating 0 1))
  ([rating offset count]
   (let [largest (max-joltage-from rating (banks) count offset)
         largest-offset (first largest)
         largest-value (second largest)
         next-offset (inc largest-offset)]
     (if (= count (banks))
       largest-value
       (parse-long (str largest-value (max-joltage rating next-offset (inc count))))))))

(defn solve [input]
  (let [spec (parse-lines input identity #"\n")]
    (reduce + (map max-joltage spec))))

(println (str "12-joltage: " (solve (read-input))))
