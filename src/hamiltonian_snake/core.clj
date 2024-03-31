(ns hamiltonian-snake.core
  (:require [hamiltonian-snake.PathUtil :as p]
            [hamiltonian-snake.Draw :as draw]
            [hamiltonian-snake.MathUtil :as v]
            [hamiltonian-snake.SnakeUtil :as s]))

(def clear-color [255 255 255 255])

(defn calc-max [spacing path-finders grid-size]
  (map #(+ (* %1 (+ %2 1)) (* %3 %2)) spacing path-finders grid-size))

(defn calc-spacing [mins maxs pad]
  (let [[x y] (v/sub maxs mins)]
    (cond
      (> x y) [pad (+ pad (- x y))]
      (< x y) [(+ pad (- y x)) pad]
      :else [pad pad])))

(defn get-offsets [maxs grid-size spacing]
  (let [xy (map #(range %3 %1 (+ %2 %3)) maxs grid-size spacing)]
    (for [x (nth xy 0) y (nth xy 1)] [x y])))

(defn finished-count [paths]
  (count (filter #(p/h-path? %) paths)))

(defn generate-h-paths [paths offsets num-paths display]
  (loop [i 0 paths paths]
    (if (and (or (nil? display) (draw/is-showing? display))
             (< (finished-count paths) (if (zero? num-paths) (count paths) num-paths)))
      (let [n-paths (pmap #(p/update-path %) paths)]
        (if (not (nil? display))
          (do (draw/clear display clear-color)
              (p/draw-paths display n-paths offsets "Generating Hamiltonian Paths")
              (draw/show display)))
        (recur (inc i) n-paths))
      (take num-paths (filter #(p/h-path? %) paths)))))

(defn scramble-h-paths [h-paths offsets num-scrambles display]
  (loop [i 0 paths h-paths]
    (if (and (or (nil? display) (draw/is-showing? display))
             (< i num-scrambles))
      (let [n-paths (pmap #(p/tail-adjacent-path-reversal % v/rand-ith) paths)]
        (if (not (nil? display))
          (do (draw/clear display clear-color)
              (p/draw-paths display n-paths offsets (str "Scrambling Hamiltonian Paths. " (- num-scrambles (inc i)) " scrambles left."))
              (draw/show display)))
        (recur (inc i) n-paths))
      paths)))

(defn generate-h-cycles [h-paths offsets num-cycles display]
  (loop [paths h-paths]
    (if (and (or (nil? display) (draw/is-showing? display))
             (< (count (filter p/cycle? paths)) num-cycles))
      (let [n-paths (pmap (fn [path] (if (p/cycle? path) path (p/tail-adjacent-path-reversal path first))) paths)]
        (if (not (nil? display))
          (do (draw/clear display clear-color)
              (draw/pause 0)
              (p/draw-paths display n-paths offsets "Converting to Hamiltonian Cycles")
              (draw/show display)))
        (recur n-paths))
      paths)))

(defn play-games [games offsets display]
  (loop [step 0 games games]
    (if (and (or (nil? display) (draw/is-showing? display)) (< (count (filter :is-finished? games)) (count games)))
      (let [u-games (pmap #(s/update-game %) games)]
        (if (not (nil? display))
          (do (draw/clear display clear-color)
              (s/draw-games display u-games offsets true (str "Playing Snake! Steps: " step))
              (draw/show display)))
        (recur (inc step) u-games))
      games)))

(def mins [0 0])
(def maxs [7 7])
(def screen-size (draw/get-screen-size))
(def width (int (- (first screen-size) 50)))
(def height (int (- (last screen-size) 100)))
(def spacing (calc-spacing mins maxs 1))
(def games-shown [8 4])
(def display-maxs (calc-max spacing games-shown (v/sub maxs mins)))
(def offsets (get-offsets display-maxs (v/sub maxs mins) spacing))

#_(let [display (draw/new-display "Hamiltonian Snake" width height mins display-maxs)
      num-paths (reduce * games-shown)
      paths (vec (repeatedly num-paths #(p/new-path mins maxs)))]
  (draw/init display clear-color)
  (let [h-paths (generate-h-paths paths offsets num-paths display)
        num-scrambles (* (:length (first h-paths)) 1)
        s-paths (scramble-h-paths h-paths offsets num-scrambles display)
        cycles (generate-h-cycles s-paths offsets num-paths display)
        games (play-games (map s/new-game cycles) offsets display)]
    (v/average (map :steps games))))