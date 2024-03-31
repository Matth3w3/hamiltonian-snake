(ns hamiltonian-snake.MathUtil)

;; Clojure code for simple neural network implementation and
;; vector/matrix operations.

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

;; region Helpful Functions
(defn outside? [[x y] [xmin ymin] [xmax ymax]]
  (or (< x xmin) (< y ymin) (>= x xmax) (>= y ymax)))

(defn average [coll]
  (/ (reduce + coll) (double (count coll))))
;;

;; region RandomUtil
(defn random
  ([min max] (+ (* (Math/random) (- max min)) min))
  ([max] (* (Math/random) max)))

(defn random-int
  ([min max] (int (random min max)))
  ([max] (int (random max))))

(defn random-vec
  ([cols min-val max-val] (repeatedly cols #(random min-val max-val)))
  ([mins maxs] (map #(random %1 %2) mins maxs))
  ([maxs] (map #(random %) maxs)))

(defn random-vec-int
  ([cols min-val max-val] (repeatedly cols #(random-int min-val max-val)))
  ([mins maxs] (map #(random-int %1 %2) mins maxs))
  ([maxs] (map #(random-int %) maxs)))

(defn rand-ith [coll]
  (if (empty? coll) nil (rand-nth coll)))

(defn random-matrix [rows cols min-val max-val]
  (vec (repeatedly rows #(random-vec cols min-val max-val))))

(defn random-matrix-int [rows cols min-val max-val]
  (vec (repeatedly rows #(random-vec-int cols min-val max-val))))

(defn random-color [] [(rand-int 256) (rand-int 256) (rand-int 256) 255])

(defn random-sat-color [] (conj (shuffle [150 0 (rand-int 150)]) 255))

(defn max-saturation [[r g b a]]
  (let [mc (max r g b)
        ratio (/ 200 mc)]
    (mapv #(min 255 (int (* % ratio))) [r g b a])))
;; endregion

;; region TRIG UTIL
(defn cos [theta] (Math/cos (Math/toRadians theta)))
(defn sin [theta] (Math/sin (Math/toRadians theta)))
;; endregion

;; region Vector Operations
(defn add [& vecs] (apply map + vecs))
(defn sub [& vecs] (apply map - vecs))
(defn mult [& vecs] (apply map * vecs))
(defn scale [v s] (map #(* % s) v))
(defn dot [& vecs] (reduce + (apply map * vecs)))
(defn dist [v1 v2] (Math/sqrt (let [d (sub v1 v2)] (dot d d))))
(defn dist? [v1 v2 d] (= (dist v1 v2) d))
(defn not-dist? [v1 v2 d] (not= (dist v1 v2) d))
(defn distSq [v1 v2] (let [d (sub v1 v2)] (dot d d)))
(defn length [v] (Math/sqrt (dot v v)))
(defn lengthSq [v] (dot v v))
(defn normalize [v] (let [l (length v)] (if (= l 0.0) v (scale v (/ 1 l)))))
(defn setLength [v len] (scale (normalize v) len))
(defn contains [vecs v] (not= (some #(= v %) vecs) nil))
(defn not-contains [vecs v] (= (some #(= v %) vecs) nil))
(defn vec-avg [& vecs] (scale (reduce add vecs) (/ 1 (count vecs))))
(defn swap [v i1 i2] (assoc v i2 (v i1) i1 (v i2)))

(defn mutate-vector [v m-rate delta]
  (map (fn [e]
         (if (> (rand) m-rate)
           e
           (if (> (rand) 0.5) (+ e delta) (- e delta))))
       v))
;; endregion

;; region Matrix Operations
(defn transpose [A] (vec (apply map vector A)))
(defn transpose-vector [v] (map (fn [x] (vec x)) v))
(defn mat-mult [A B] (let [BT (transpose B)] (map (fn [row] (map #(dot row %) BT)) A)))
(defn mat-vec-mult [A v] (map (fn [row] (dot row v)) A))
(defn mat-power [A x] (reduce (fn [B C] (mat-mult C B)) (repeat x A)))
(defn rot-mat [theta phi]
  [[(cos theta) (- (sin phi))]
   [(sin theta) (cos phi)]])
(defn rotate [v theta] (mat-vec-mult (rot-mat theta theta) v))

(defn mutate-matrix [matrix m-rate delta]
  (map (fn [row] (mutate-vector row m-rate delta)) matrix))
;; endregion

;; region Neural Network
(defn dense [inputSize outputSize]
  (let [range 1]
    {:weights (random-matrix outputSize inputSize 0 range)
     :bias (random-vec outputSize 0 range) :bias-size outputSize
     :w-rows outputSize :w-cols inputSize}))

(defn forward-dense [dense input] (add (mat-vec-mult (:weights dense) input) (:bias dense)))

(defn mutate-dense [{:keys [weights bias] :as dense} m-rate delta]
  (assoc dense :weights (mutate-matrix weights m-rate delta)
               :bias (mutate-vector bias m-rate delta)))

(defn soft-max [input]
  (let [output (map #(Math/exp  %) input)] (scale output (/ 1 (reduce + output)))))
(defn tanh [input] (map #(Math/tanh %) input))
(defn sigmoid [input] (map #(/ 1 (+ 1 (Math/exp (* % -1)))) input))

(defn new-network [layers activations]
  {:layers (vec (map (fn [layer] (dense (get layer 0) (get layer 1))) layers))
   :activations activations})

(defn forward-prop [layers activations input]
  (loop [i 0 output input]
    (if (< i (count layers))
      (recur (inc i)
             ((nth activations i) (forward-dense (nth layers i) output)))
      output)))

(defn predict [network input]
  (forward-prop (:layers network) (:activations network) input))

(defn mutate-network [{:keys [layers] :as nn} m-rate delta]
  (assoc nn :layers (map #(mutate-dense % m-rate delta) layers)))
;; endregion

;; endregion