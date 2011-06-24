(ns sbfield.core
  (:gen-class)
  (:require [sbfield.debug :as d]
            [sbfield.interval-tree :as i]
            [clojure.contrib.seq :as s]
            [clojure.contrib.math :as math]
            [clojure.contrib.command-line :as cl]
            [clojure.data.finger-tree :as ft]
            [clojure.java.io :as io])
  (:import [java.io PushbackReader BufferedWriter])
  (:use iterate))

(def *population-size* 1000)
(def *number-of-generations* 1000)
(def *mutation-probability* 0.1)

(defn generate-random-field
  ([m n n-rect]
     (let [x (make-array Boolean/TYPE m n)]
       (dotimes [i m]
         (dotimes [j n]
           (aset x i j (> (rand-int 100) 50))))
       {:field-array x :n-rectangles n-rect :dims [m n]}))
  ([m n] (generate-random-field m n (rand-int 11)))
  ([m] (generate-random-field m m))
  ([] (generate-random-field (rand-int 50) (rand-int 50))))

(defn display-strawberry-field [{:keys [field-array n-rectangles]}]
  (println n-rectangles)
  (println (d/thrush-with-sym [x]
             #(str (apply str (map (fn [c] (if c "@" ".")) %)) "\n")
             (map x field-array) (apply str x))))

(defn read-strawberry-field [fname]
  (with-open [rdr (clojure.java.io/reader fname)]
    (d/thrush-with-sym [x]
      (line-seq rdr)
      (let [[num-str & fields-str] x
             field-array (into-array (keep #(if-let [v (seq (keep {\@ true \. false} %))]
                                              (into-array Boolean/TYPE v)) fields-str))
             dims [(alength field-array) (alength (aget field-array 0))]]
        {:n-rectangles (read-string num-str)
         :field-array field-array
         :dims dims})))) 

(defn decorate-strawberry-field-with-chromosomes [{:keys [n-rectangles field-array dims] :as strawberry-field}]
  (let [[m n] dims
        chromosome-id-to-coords-map (vec (keep identity (for [i (range m) j (range n)]
                                                          (if (aget field-array i j) [i j]))))
        coords-to-chromosome-id-map (into {} (map-indexed #(vector %2 %1) chromosome-id-to-coords-map))]
    (-> strawberry-field
        (assoc :chromo-pos-to-coord chromosome-id-to-coords-map)
        (assoc :coord-to-chromo-pos coords-to-chromosome-id-map)
        (assoc :population-size *population-size*)
        (assoc :chromo-length (* (dec n-rectangles) 3)))))

(defn range-loc [[min max] r]
  {:pre [(<= min max) r min max]}
  "return a value from min to max inclusive of both ends depending on r "
  (let [d (/ 1.0 (+ 1 max (- min)))
        loc (+ min (int (/ r d)))] loc))

(defn generate-random-feasible-population [{:keys [chromo-length dims coord-to-chromo-pos n-rectangles population-size] :as field}]
  (let [random-feasible-sol (fn [] (apply concat (repeatedly (dec n-rectangles) #(do [([:i :j] (rand-int 2)) (rand) (rand)]))))]
    (repeatedly population-size random-feasible-sol)))

(defn mutate [chromo]
  (if (<= (rand) *mutation-probability*)
    (let [v (vec chromo) n (count v) ni (rand-int n)]
      (update-in v [ni]
                 #(cond
                   (= % :i) :j
                   (= % :j) :i
                   true (let [x (({0 + 1 -} (rand-int 2)) % (rand *mutation-probability*))]
                          (cond
                           (< x 0) 0.0 (> x 0.9999999) 0.9999999 true x)))))
    chromo))
  
          

(defn cross [chromo1 chromo2]
  (let [cross-over-point (rand-int (min (count chromo1) (count chromo2)))
        [[c-1-1 c-1-2] [c-2-1 c-2-2]] (map #(split-at cross-over-point %) [chromo1 chromo2])]
    (map (partial apply concat) [[c-1-1 c-2-2] [c-2-1 c-1-2]])))


(defn bounding-box [coords]
  (reduce (fn [[[i-min j-min] [i-max j-max] :as w] [i j]]
            (if w [[(min i i-min) (min j j-min)]
                   [(max i i-max) (max j j-max)]] [[i j] [i j]])) nil coords))

(defn bounding-box-rectangle [{:keys [chromo-pos-to-coord]}]
  (bounding-box chromo-pos-to-coord))

(defn rectangle-split-and-shrink [dir [[[i-min j-min] [i-max j-max]] coords] ratio]
  {:pre [dir i-min i-max j-min j-max coords ratio]}
  (keep identity
        (cond
         (= dir :i) (let [x (range-loc [i-min i-max] ratio)
                          {:keys [g1 g2]} (group-by #(if (<= (first %) x) :g1 :g2) coords)]
                      [[(bounding-box g1) g1] (if (seq g2) [(bounding-box g2) g2])])
         (= dir :j) (let [x (range-loc [j-min j-max] ratio)
                          {:keys [g1 g2]} (group-by #(if (<= (second %) x) :g1 :g2) coords)]
                      [[(bounding-box g1) g1] (if (seq g2) [(bounding-box g2) g2])]))))

(defn find-containing-rectangle [rects [i j]]
  {:pre [#_(do (clojure.pprint/pprint rects) (println [i j]) true)]
   :post [%]}
  (some (fn [[id [[i-min j-min] [i-max j-max]]]]
          (if (nil? id) (throw (Exception. "error in find-containing-rectangle")))
          (if (and (<= i-min i i-max) (<= j-min j j-max)) id)) rects))   

(defn find-rectangles [{[m n] :dims :keys [field-array chromo-pos-to-coord] :as w} sol]
  {:post [#_(if-not % (do (println %) (println sol)) true)]}
  (d/thrush-with-sym [x] sol
    (partition 3 x)
    (loop [[[dir rect-id-ratio range-ratio :as cur-triplet] & xs] x rects {0 [(bounding-box-rectangle w) chromo-pos-to-coord]}]
      (if (nil? cur-triplet) rects
          (let [rct-id (range-loc [0 (dec (count rects))] rect-id-ratio)
                [r1 r2] (rectangle-split-and-shrink dir (rects rct-id) range-ratio)
                updated-rects (d/thrush-with-sym [rct] rects
                                (assoc rct rct-id r1)
                                (if r2 (assoc rct (count rct) r2) rct))]
            (recur xs updated-rects))))))
        
(defn rect-area [[[i-min j-min] [i-max j-max] :as w]]
  (if w (* (+ i-max 1 (- i-min)) (+ j-max 1 (- j-min))) 0))

(defn calc-quality [{:keys [chromo-pos-to-coord] :as w} sol]
  (let [rects (find-rectangles w sol)]
    (d/thrush-with-sym [x] rects
      (map (fn [[_ [[[i-min j-min] [i-max j-max] :as w]]]]
             (+ 10 (rect-area w))) x)
      {:cost (apply + 0.0  x) :rects rects})))

(defn next-generation [{:keys [chromo-length population-size] :as field} population costs]
  (let [quality (map #(/ 1 %) costs)
        quality-sol-map (into (sorted-map)
                              (map vector (reductions + quality) population))
        max-quality-sol-map-key (ffirst (rseq quality-sol-map))
        choose-parent #(fnext (first (subseq quality-sol-map >= (rand max-quality-sol-map-key))))
        offspring #(cross (choose-parent) (choose-parent))]
    (map mutate (apply concat (repeatedly (/ population-size 2) offspring)))))

(defn display-solution [{:keys [dims] :as w} [sol {:keys [cost rects]}]]
  {:pre [#_(not (if (some nil? (keys rects)) (do (println rects) true))) #_(and dims (seq sol)) #_(do (println sol) true)]}
  (let [[m n] dims
        chars (vec "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        field (into-array (repeatedly m #(char-array n \.)))
        _ (reduce (fn [fld [rect-id [[[i-min j-min] [i-max j-max]]]]]
                    (dorun (for [i (range i-min (inc i-max))
                                 j (range j-min (inc j-max))]
                             (do (if (nil? rect-id) (throw (Exception. "hello")))
                                 (aset fld i j (chars rect-id))))) fld)
                  field rects)]
    (println ['cost cost 'number-of-rectangles (count rects)])
    (println (apply str (map #(str (apply str %) "\n") field)))))

(defn evolve-solution [field number-of-generations]
  (loop [i-gen 0 best-known-solution nil
         current-generation-of-solutions (generate-random-feasible-population field)]
    (let [quality-metrics (doall (map (partial calc-quality field) current-generation-of-solutions))
          solution-quality-metric-pairs (d/thrush-with-sym [x] (map vector current-generation-of-solutions quality-metrics)
                                          (if best-known-solution (conj x best-known-solution) x))
          current-best-solution (apply min-key (comp :cost second) solution-quality-metric-pairs)]
      (when (not= current-best-solution best-known-solution) (println ['i-gen i-gen]) (display-solution field current-best-solution))
      (if (< i-gen number-of-generations)
        (recur (inc i-gen) current-best-solution
               (next-generation field current-generation-of-solutions (map :cost quality-metrics)))
        current-best-solution))))

(defn solve
  ([fname]
     (d/thrush-with-sym [x]
       (read-strawberry-field fname)
       (do (display-strawberry-field x) x)
       (decorate-strawberry-field-with-chromosomes x)
       [x (evolve-solution x *number-of-generations*)]
       (let [[field [_ {:keys [cost]} :as solution]] x]
         (with-open [r (BufferedWriter. (io/writer (str fname ".solution")))]
           (binding [*out* r]
             (display-strawberry-field field)
             (display-solution field solution)))
         cost)))
  ([]
     (d/thrush-with-sym [x]
       (generate-random-field 3 3 2)
       (do (display-strawberry-field x) x)
       (decorate-strawberry-field-with-chromosomes x)
       (evolve-solution x 100))))

(defn report [fname]
  (let [files (map #(str "/home/sunil/work/lisp/ita/strawberry/f" % ".txt") (range 1 10))
        total-cost (reduce + (map solve files))]
    (spit fname (str "total cost " total-cost "\n"))
    (spit fname (apply str (map #(slurp (str % ".solution")) files)) :append true)))

(defn -main [& args]
  (cl/with-command-line args
    "usage : sbfield [-ps <int>] [-ng <int>] [-o <string>] [-cfp <string>]"
    [[population-size ps "size of the population in each generation " 1000]
     [number-of-generations ng "number of generations the solution has to evolve " 1000]
     [out-fname o "name of the output file to which solutions to all the problems are written " "strawberry.out.txt"]
     [custom-problem-filename fcp "if you want to run on a different problem apart from the 9 problems already posted on your website .. then input the file containing the problem in a format similar to any of f<1-9>.txt " nil]]
    (binding [*population-size* (if (string? population-size) (read-string population-size) population-size)
              *number-of-generations* (if (string? number-of-generations) (read-string number-of-generations) number-of-generations)]
      (if custom-problem-filename
        (solve custom-problem-filename) (report out-fname)))))
    


