(ns aoc2025012
  (:require [clojure.string :as str]))

(defn read-input []
  (let [file-name (nth *command-line-args* 0)]
    (str/trim-newline (slurp file-name))))

(defn parse-lines
  [input & [parse-fn nl-sep]]
  (mapv #(parse-fn %)
        (str/split input nl-sep)))

(defn parse-fn
  [line]
  (let [[_ strdir stramount] (re-matches #"^([A-Za-z])(\d+)$" line)]
    [(case strdir "L" :left "R" :right) (parse-long stramount)]))

(defn rotate [val dir amount]
  ;(assoc (assoc info :zeroes (inc (:zeroes info))) info) :pos newpos)))
  (let [offset (case dir :left (- amount) :right amount)
        posoffset (+ val offset)
        zeroes (quot (abs posoffset) 100)
        newpos (mod posoffset 100)
        newzeroes (if (and (<= posoffset 0) (not= val 0)) (inc zeroes) zeroes)]
    {:pos newpos :zeroes newzeroes }))

(defn rotate-and-count [info step]
  (let [rotinfo (apply rotate (:pos info) step)]
    (let [pos (:pos rotinfo)
          zeroes (+ (:zeroes info) (:zeroes rotinfo))]
      { :pos pos :zeroes zeroes })))

(defn solve [input]
  (let [steps (parse-lines input parse-fn #"\n")]
    (:zeroes (reduce rotate-and-count {:pos 50 :zeroes 0} steps))))

(println (str "Click zeroes: " (solve (read-input))))