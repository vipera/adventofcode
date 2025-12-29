(ns aoc2025011
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
  (let [offset (case dir :left (- amount) :right amount)]
    (mod (+ val offset) 100)))

(defn rotate-and-count [info step]
  (let [newpos (apply rotate (:pos info) step)]
    (assoc (if (= 0 newpos) (assoc info :zeroes (inc (:zeroes info))) info) :pos newpos)))

(defn solve [input]
  (let [steps (parse-lines input parse-fn #"\n")]
    (:zeroes (reduce rotate-and-count {:pos 50 :zeroes 0} steps))))

(println (str "Zeroes: " (solve (read-input))))
