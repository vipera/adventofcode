(ns aoc2025022
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

(defn repeat-subid [substr stridlen]
  (let [substrlen (.length substr)
        amount (quot stridlen substrlen)]
    (apply str (repeat amount substr))))

(defn generate-subids [strid]
  (let [stridlen (.length strid)
        halflen (quot stridlen 2)]
    (map #(repeat-subid % stridlen) (map #(subs strid 0 %) (range 1 (inc halflen))))))

(defn invalid-id? [id]
  (let [strid (str id)]
    (some #(= strid %) (generate-subids strid))))

(defn solve [input]
  (let [steps (parse-lines input parse-fn #",")]
    (reduce + (mapcat #(filterv invalid-id? (range (first %) (inc (last %)))) steps))))

(println (str "Sum: " (solve (read-input))))
