(ns aoc2025021
  (:require [clojure.string :as str]))

(defn read-input []
  (let [file-name (nth *command-line-args* 0)]
    (str/trim-newline (slurp file-name))))

(defn parse-lines
  [input & [parse-fn nl-sep]]
  (mapv #(parse-fn %)
        (str/split input nl-sep)))

(defn parse-fn
  [part]
  (let [[_ strfrom strto] (re-matches #"^(\d+)-(\d+)$" part)]
    [(parse-long strfrom) (parse-long strto)]))

(defn invalid-id? [id]
  (let [strid (str id)
        stridlen (.length strid)
        halflen (int (/ stridlen 2))
        firsthalf (subs strid 0 halflen)
        lasthalf (subs strid halflen)]
    (if (odd? stridlen) false (= firsthalf lasthalf))))

(defn solve [input]
  (let [steps (parse-lines input parse-fn #",")]
    (reduce + (mapcat #(filterv invalid-id? (range (first %) (inc (last %)))) steps))))

(println (str "Sum: " (solve (read-input))))
