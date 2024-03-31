(ns hamiltonian-snake.PathUtil
  (:require [hamiltonian-snake.MathUtil :as v]
            [hamiltonian-snake.Draw :as draw]))

;; Clojure code for finding hamiltonian cycles/paths in an n by m grid.

(def moves [[1 0] [0 1] [-1 0] [0 -1]])

(defn valid-moves [mins maxs nodes index moves]
  (remove #(or (v/contains nodes %)
               (v/outside? % mins maxs))
          (mapv #(v/add (nth nodes index) %) moves)))

(defn adjacent-indices [nodes]
  (filter #(<= (v/dist (nth nodes %) (last nodes)) 1) (range (- (count nodes) 2))))

(defn tail-adjacent-path-reversal [{:keys [nodes tail-list] :as path} choice-function]
  (let [b-indices (filter #(v/not-contains tail-list (nth nodes (inc %))) (adjacent-indices nodes))
        index (choice-function (sort-by #(v/dist (first nodes) (nth nodes (inc %))) b-indices))
        switch? (nil? index)
        n-nodes (if switch?
                  (reverse nodes)
                  (concat (take (inc index) nodes) (reverse (drop (inc index) nodes))))
        n-tail-list (if switch? [(first nodes)] (cons (last n-nodes) tail-list))]
    (assoc path :nodes n-nodes :length (count n-nodes) :tail-list n-tail-list)))

(defn cycle? [{:keys [nodes]}]
  (<= (v/dist (first nodes) (last nodes)) 1))

(defn h-path? [{:keys [m-size length]}]
  (= m-size length))

(defn new-graph [[xmin ymin] [xmax ymax]]
  (for [x (range xmin xmax) y (range ymin ymax)] [x y]))

(defn corner-nodes [[x-min y-min] [x-max y-max]]
  [[x-min y-min]
   [x-min (dec y-max)]
   [(dec x-max) (dec y-max)]
   [(dec x-max) y-min]])

(defn new-path [mins maxs]
  (let [graph (new-graph mins maxs)
        nodes [(rand-nth (corner-nodes mins maxs))]]
    {:mins mins :maxs maxs
     :m-size (count graph)
     :scramble-counter 0
     :nodes nodes
     :moves (shuffle moves)
     :tail-list []
     :graph graph
     :squares (filter (fn [[x y]] (or (and (even? x) (even? y)) (and (odd? x) (odd? y)))) graph)
     :length (count nodes)
     :color (v/random-sat-color)}))

(defn update-path [{:keys [nodes length mins maxs m-size moves] :as path}]
  (if (= m-size length)
    path
    (let [v-moves (valid-moves mins maxs (:nodes path) 0 moves)]
      (if (zero? (count v-moves))
        (let [r-nodes (reverse nodes)]
          (if (zero? (count (valid-moves mins maxs r-nodes 0 moves)))
            (tail-adjacent-path-reversal path last)
            (assoc path :nodes r-nodes)))
        (assoc path :nodes (cons (first v-moves) nodes) :length (inc length) :tail-list [])))))

(defn draw-graph [display offset squares]
  (draw/color display [240 240 240 255])
  (doseq [[x y] squares]
    (draw/fill-square display (v/add offset [x y]) 0.5)))

(defn draw-tail-list [display tail-list offset]
  (draw/color display [255 0 0 100])
  (dotimes [i (count tail-list)]
    (draw/fill-square display (v/add offset (nth tail-list i)) 0.4)))

(defn draw-path [display {:keys [color nodes length squares]} offset]
  (draw-graph display offset squares)
  (draw/stroke display 5)
  (draw/color display color)
  (draw/polyline display (map #(v/add % offset) nodes) length)
  (draw/stroke display 1)
  (draw/fill-square display (v/add offset (first nodes)) 0.4)
  (draw/fill-circle display (v/add offset (last nodes)) 0.3))

(defn draw-paths [display paths offsets title-text]
  (dotimes [i (min (count paths) (count offsets))]
    (draw-path display (nth paths i) (nth offsets i)))
  (draw/color display [0 0 0 255])
  (draw/text display (v/mult (:maxs display) [0.5 0.98]) title-text))