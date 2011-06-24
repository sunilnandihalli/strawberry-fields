(ns sbfield.interval-tree
  (:require [sbfield.debug :as d])
  (:use iterate)
  #_(:import [intervalTree IntervalTree]))

(comment
  (defn add-interval [interval-tree [[start end] data]]
    (.addInterval interval-tree start end data))
  (defn interval-tree [& [[[start end] data] :as itrvls]]
    (let [tree (IntervalTree.)]
      (dorun (for [x itrvls]
               (add-interval tree x)))
      tree))
  (defn get-intervals [interval-tree [start end]]
    (d/thrush-with-sym [x]
      (.getIntervals interval-tree 0 100)
      (map #(vector [(.getStart %) (.getEnd %)] (.getData %)) x)))
  (defn display-interval-tree [itrv-tree]
    (d/thrush-with-sym [x]
      itrv-tree
      (get-intervals x [0 100])
      (println x))))

(defn interval-tree [& [[[start end] data] :as itrvls]]
  (into #{} itrvls))

(defn add-interval [interval-tree [[start end] data :as w]]
  (conj interval-tree w))

(defn get-intervals [interval-tree [start end]]
  (filter (fn [[[i-start i-end] i-data :as i-w]]
            (or (some #(<= i-start % i-end) [start end])
                (some #(<= start % end) [i-start i-end]))) interval-tree))

(defn add-non-overlapping-interval [interval-tree [[start end] data :as w]]
  (if-not (seq (get-intervals interval-tree [start end]))
    (conj interval-tree w)))

(defn calc-interval-overlap [[[s1 e1] d1 :as w1] [[s2 e2] d2 :as w2]]
  (let [overlap [(cond (<= s1 s2 e1) s2 (<= s2 s1 e2) s1)
                 (cond (<= s1 e2 e1) e2 (<= s2 e1 e2) e1)]]
    (if (every? identity overlap) [overlap [d1 d2]])))

#_(calc-interval-overlap [[1 3] :a] [[2 4] :b])
#_(calc-interval-overlap [[1 6] :a] [[2 4] :b])
#_(calc-interval-overlap [[3 6] :a] [[2 4] :b])
#_(calc-interval-overlap [[2 4] :a] [[1 6] :b])
#_(calc-interval-overlap [[0 40] :a] [[1 6] :b])
   
(defn get-overlaps [interval-tree [[start end] data :as w]]
  (keep #(calc-interval-overlap w %) interval-tree))

(defn add-interval-returning-overlapping-intervals [interval-tree [[start end] data :as w]]
  (let [x (seq (get-overlaps interval-tree w))]
    [(conj interval-tree w) x]))

(defn remove-interval-returning-overlapping-intervals [interval-tree [[start end] data :as w]]
  (d/thrush-with-sym [x] interval-tree
    (disj x w) [x (get-overlaps x w)]))
    
    

(defn remove-interval [interval-tree w]
  (disj interval-tree w))
    