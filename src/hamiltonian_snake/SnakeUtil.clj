(ns hamiltonian-snake.SnakeUtil
  (:require [hamiltonian-snake.MathUtil :as v]
            [hamiltonian-snake.Draw :as draw]))

;; Clojure code for the snake game utilizing hamiltonian cycles to win

(def moves [[0 1] [0 -1] [-1 0] [1 0]])

(defn valid-moves [mins maxs nodes target]
  (remove #(or (if (or (= % target) (= (count nodes) 2))
                 (v/contains nodes %)
                 (v/contains (drop-last nodes) %)
                 )
               (v/outside? % mins maxs))
          (mapv #(v/add (nth nodes 0) %) moves)))

(defn finished? [game] (= (:m-size game) (count (:nodes (:snake game)))))

(defn new-target [graph snake]
  (let [p-squares (remove #(v/contains (:nodes snake) %) graph)]
    (if (empty? p-squares) nil (rand-nth p-squares))))

(defn maintain-path [nodes path dir]
  (let [i (.indexOf path (last nodes))]
    (concat (drop (mod (inc i) (count path)) path)
            (take (mod (inc i) (count path)) path))
    ;(if (= dir 1)
    ;  (concat (reverse (take (mod (inc i) (count path)) path))
    ;          (reverse (drop (mod (inc i) (count path)) path)))
    ;  (concat (drop (mod (inc i) (count path)) path)
    ;          (take (mod (inc i) (count path)) path)))
    )
  )

(defn new-snake [nodes path color]
  {:nodes nodes
   :initial nodes
   :eat-timer 0
   :dir-lock-timer 2
   :dir -1
   :path (maintain-path nodes path -1)
   :color color
   :eat-color (v/max-saturation color)})

(defn new-game [{:keys [graph mins maxs m-size nodes squares color]}]
  (let [center (map int (v/vec-avg mins maxs))
        c-index (.indexOf nodes center)
        snake (new-snake [(nth nodes (mod (+ c-index 1) m-size)) (nth nodes c-index)] nodes color)]
    {:mins mins :maxs maxs
     :graph graph
     :m-size m-size
     :squares squares
     :is-finished? false
     :score 2
     :steps 0
     :snake snake
     :target (new-target graph snake)}))

(defn shortcut? [move nodes path target]
  (let [i1 (.indexOf path (first nodes))
        i2 (.indexOf path (last nodes))
        i3 (.indexOf path move)
        t1 (.indexOf path target)]
    (and (< i3 i1 i2) (<= t1 i3))))

(defn update-snake [{:keys [nodes path dir eat-timer] :as snake} target mins maxs]
  (let [v-moves (valid-moves mins maxs nodes target)]
    (if (= (count v-moves) 0)
      (assoc snake :nodes (:initial snake))
      (let [n-index (mod (+ (.indexOf path (first nodes)) dir) (count path))
            short-cuts (filter #(shortcut? % nodes path target) v-moves)
            p-moves (cons (nth path n-index) short-cuts)
            stay? (or (empty? short-cuts) (>= (count nodes) (* 0.6 (count path))))
            move (if stay?
                   (nth path n-index)
                   (first (sort-by #(.indexOf path %) p-moves)))
            n-nodes (cons move (if (= move target) nodes (drop-last nodes)))
            n-path (maintain-path n-nodes path dir)
            n-timer (if (= move target) 5 (max 0 (dec eat-timer)))]
        (assoc snake :nodes n-nodes :path n-path :eat-timer n-timer)))))

(defn update-target [target snake graph]
  (if (v/contains (:nodes snake) target) (new-target graph snake) target))

(defn update-game [{:keys [snake mins maxs graph target is-finished? steps] :as game}]
  (if (or is-finished? (finished? game))
    (assoc game :is-finished? true)
    (let [n-snake (update-snake snake target mins maxs)
          n-target (update-target target n-snake graph)]
      (assoc game :snake n-snake :target n-target :steps (inc steps)
                  :score (count (:nodes n-snake))))))

(defn draw-graph [display offset squares]
  (draw/color display [240 240 240 255])
  (doseq [[x y] squares]
    (draw/fill-square display (v/add offset [x y]) 0.5)))

(defn draw-game [display {:keys [snake target squares is-finished?]} offset show-path?]
  (draw-graph display offset squares)
  (if show-path?
    (do
      (draw/color display (v/scale (:color snake) 0.5))
      (draw/polyline display (map #(v/add % offset) (:path snake)) (count (:path snake)))
      (draw/polyline display (map #(v/add % offset) [(first (:path snake)) (last (:path snake))]) 2)))
  (draw/stroke display 5)
  (if (or is-finished? (> (:eat-timer snake) 0))
    (draw/color display (:eat-color snake))
    (draw/color display (:color snake)))
  (draw/polyline display (map #(v/add % offset) (:nodes snake)) (count (:nodes snake)))
  (draw/stroke display 1)
  (draw/fill-square display (v/add offset (first (:nodes snake))) 0.4)
  (if (not (nil? target))
    (do
      (draw/color display (:eat-color snake))
      (draw/fill-circle display (v/add target offset) 0.4))))

(defn draw-games [display games offsets show-path? title-text]
  (dotimes [i (min (count games) (count offsets))]
    (draw-game display (nth games i) (nth offsets i) show-path?))
  (draw/color display [0 0 0 255])
  (draw/text display (v/mult (:maxs display) [0.5 0.98]) title-text))