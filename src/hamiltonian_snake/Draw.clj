(ns hamiltonian-snake.Draw
  (:import (javax.swing ImageIcon JLabel JFrame)
           (java.awt RenderingHints Graphics2D Color Font BasicStroke Toolkit)
           (java.awt.image BufferedImage)
           (javax.swing SwingUtilities)
           (java.awt.geom Ellipse2D Ellipse2D$Double Line2D$Double Rectangle2D$Double)
           (java.awt.event KeyListener KeyEvent)))

;; Clojure code for 2D drawing and animation
;; Based on StdDraw.java by Princeton University

(def FPS-LIMIT 60)
(def MSPF (* (/ 1 FPS-LIMIT) 1000))
(defn fps [t1] (/ 1000.0 (- (System/currentTimeMillis) t1)))
(def current-time (atom (System/currentTimeMillis)))

(defn get-screen-size []
  (let [size (.getScreenSize (Toolkit/getDefaultToolkit))]
    [(.getWidth size) (.getHeight size)]))

(def keys-down (atom []))

(defn is-key-pressed? [key-code]
  (let [down (deref keys-down)]
    (not= (some #(= key-code %) down) nil)))

(defn is-enter-pressed? []
  (is-key-pressed? (KeyEvent/VK_ENTER)))

(defn is-space-pressed? []
  (is-key-pressed? (KeyEvent/VK_SPACE)))

(defn remove-key [key-code]
  (SwingUtilities/invokeLater
    #(let [temp-down (deref keys-down)]
       (reset! keys-down
               (remove (fn [k] (= k key-code)) temp-down)))))

(defn add-key [key-code]
  (SwingUtilities/invokeLater
    #(let [temp-down (deref keys-down)]
       (if (not (is-key-pressed? key-code))
         (reset! keys-down (cons key-code temp-down))))))

(defn new-display [title width height mins maxs]
  (let [onscreen-image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        offscreen-image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        onscreen (.createGraphics onscreen-image)
        font (Font. "SansSerif" Font/PLAIN 16)
        offscreen (.createGraphics offscreen-image)
        frame (JFrame. (str title))
        content-pane (JLabel. (ImageIcon. onscreen-image))]
    (doto offscreen
      (.addRenderingHints
        (doto (RenderingHints. RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
          (.put RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_DEFAULT)))
      (.setStroke (BasicStroke. 1 BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))
      (.setFont font))
    {:frame frame :content content-pane :font font
     :onscreen-image onscreen-image :offscreen-image offscreen-image
     :onscreen onscreen :offscreen offscreen
     :width width :height height :mins mins :maxs maxs}))

(defn to-color [[r g b a]] (Color. (int r) (int g) (int b) (int a)))

(defn init [{:keys [^JFrame frame content]} bg-color]
  (doto frame
    (.setContentPane content)
    (.addKeyListener
      (reify KeyListener
        (keyTyped [this e] )
        (keyPressed [this e] (add-key (.getKeyCode e)))
        (keyReleased [this e] (remove-key (.getKeyCode e)))))
    (.setBackground (to-color bg-color))
    (.setLocation 0 0) (.setResizable true)
    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
    (.pack) (.requestFocusInWindow)
    (.setExtendedState JFrame/MAXIMIZED_BOTH)
    (.setVisible true)))

(defn is-showing? [display] (and (not (nil? display)) (.isShowing ^JFrame (:frame display))))

(defn stroke [{:keys [^Graphics2D offscreen]} stroke]
  (.setStroke offscreen (BasicStroke. stroke BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND)))

(defn scaleX [x w xmin xmax]
  (/ (* w (- x xmin)) (- xmax xmin)))
(defn scaleY [y h ymin ymax]
  (/ (* h (- ymax y)) (- ymax ymin)))
(defn scaleXY [[x y] w h [xmin ymin] [xmax ymax]]
  [(scaleX x w xmin xmax) (scaleY y h ymin ymax)])

(defn factorY [y h ymin ymax]
  (/ (* y h) (Math/abs (double (- ymax ymin)))))
(defn factorX [x w xmin xmax]
  (/ (* x w) (Math/abs (double (- xmax xmin)))))
(defn factorXY [[x y] w h [xmin ymin] [xmax ymax]]
  [(factorX x w xmin xmax) (factorY y h ymin ymax)])

(defn userX [x w xmin xmax]
  (+ xmin (* x (- xmax xmin) (/ 1 w))))
(defn userY [y h ymin ymax]
  (- ymax (* y (- ymax ymin) (/ 1 h))))
(defn userXY [[x y] w h [xmin ymin] [xmax ymax]]
  [(userX x w xmin xmax) (userY y h ymin ymax)])

(defn pause [t] (Thread/sleep t))

(defn color [display color]
  (.setColor ^Graphics2D (:offscreen display) ^Color (to-color color)))

(defn clear [{:keys [^Graphics2D offscreen width height]} [r g b a]]
  (reset! current-time (System/currentTimeMillis))
  (doto offscreen (.setColor (Color. (int r) (int g) (int b) (int a)))
                  (.fillRect 0 0 width height)))

(defn show [{:keys [^Graphics2D onscreen ^BufferedImage offscreen-image frame]}]
  (pause 0)
  (.drawImage onscreen offscreen-image 0 0 nil)
  (.repaint ^JFrame frame))

(defn line [{:keys [^Graphics2D offscreen mins maxs width height]} p1 p2]
  (let [[x1 y1] (scaleXY p1 width height mins maxs)
        [x2 y2] (scaleXY p2 width height mins maxs)]
    (.draw offscreen (Line2D$Double. x1 y1 x2 y2))))

(defn pixel [^Graphics2D offscreen ^double x ^double y]
  (.fillRect offscreen (int (Math/round x)) (int (Math/round y)) 1 1))

(defn circle [{:keys [^Graphics2D offscreen mins maxs width height]} point radius]
  (let [[xs ys] (scaleXY point width height mins maxs)
        [ws hs] (factorXY (repeat 2 (* radius 2)) width height mins maxs)]
    (if (and (<= ws 1) (<= hs 1))
      (pixel offscreen xs ys)
      (.draw offscreen (Ellipse2D$Double. (- xs (/ ws 2)) (- ys (/ hs 2)) ws hs)))))

(defn fill-circle [{:keys [^Graphics2D offscreen mins maxs width height]} point radius]
  (let [[xs ys] (scaleXY point width height mins maxs)
        [ws hs] (factorXY (repeat 2 (* radius 2)) width height mins maxs)]
    (if (and (<= ws 1) (<= hs 1))
      (pixel offscreen xs ys)
      (.fill offscreen (Ellipse2D$Double. (- xs (/ ws 2)) (- ys (/ hs 2)) ws hs)))))

(defn square [{:keys [^Graphics2D offscreen mins maxs width height]} point half-length]
  (let [[xs ys] (scaleXY point width height mins maxs)
        [ws hs] (factorXY (repeat 2 (* half-length 2)) width height mins maxs)]
    (if (and (<= ws 1) (<= hs 1))
      (pixel offscreen xs ys)
      (.draw offscreen (Rectangle2D$Double. (- xs (/ ws 2)) (- ys (/ hs 2)) ws hs)))))

(defn fill-square [{:keys [^Graphics2D offscreen mins maxs width height]} point half-length]
  (let [[xs ys] (scaleXY point width height mins maxs)
        [ws hs] (factorXY (repeat 2 (* half-length 2)) width height mins maxs)]
    (if (and (<= ws 1) (<= hs 1))
      (pixel offscreen xs ys)
      (.fill offscreen (Rectangle2D$Double. (- xs (/ ws 2)) (- ys (/ hs 2)) ws hs)))))

(defn rectangle [{:keys [^Graphics2D offscreen mins maxs width height]} point half-width half-height]
  (let [[xs ys] (scaleXY point width height mins maxs)
        [ws hs] (factorXY [(* half-width 2) (* half-height 2)] width height mins maxs)]
    (if (and (<= ws 1) (<= hs 1))
      (pixel offscreen xs ys)
      (.draw offscreen (Rectangle2D$Double. (- xs (/ ws 2)) (- ys (/ hs 2)) ws hs)))))

(defn fill-rectangle [{:keys [^Graphics2D offscreen mins maxs width height]} point half-width half-height]
  (let [[xs ys] (scaleXY point width height mins maxs)
        [ws hs] (factorXY [(* half-width 2) (* half-height 2)] width height mins maxs)]
    (if (and (<= ws 1) (<= hs 1))
      (pixel offscreen xs ys)
      (.fill offscreen (Rectangle2D$Double. (- xs (/ ws 2)) (- ys (/ hs 2)) ws hs)))))

(defn polygon [{:keys [^Graphics2D offscreen mins maxs width height]} points n-points]
  (let [xys (map (fn [v] (scaleXY v width height mins maxs)) points)
        xs (map (fn [v] (int (nth v 0))) xys)
        ys (map (fn [v] (int (nth v 1))) xys)]
    (.drawPolygon offscreen (int-array xs) (int-array ys) n-points)))

(defn fill-polygon [{:keys [^Graphics2D offscreen mins maxs width height]} points n-points]
  (let [xys (map (fn [v] (scaleXY v width height mins maxs)) points)
        xs (map (fn [v] (int (nth v 0))) xys)
        ys (map (fn [v] (int (nth v 1))) xys)]
    (.fillPolygon offscreen (int-array xs) (int-array ys) n-points)))

(defn polyline [{:keys [^Graphics2D offscreen mins maxs width height]} points n-points]
  (let [xys (map (fn [v] (scaleXY v width height mins maxs)) points)
        xs (map (fn [v] (int (nth v 0))) xys)
        ys (map (fn [v] (int (nth v 1))) xys)]
    (.drawPolyline offscreen (int-array xs) (int-array ys) n-points)))

(defn text [{:keys [^Graphics2D offscreen font mins maxs width height]} point text]
  (.setFont offscreen font)
  (let [metrics (.getFontMetrics offscreen)
        [xs ys] (scaleXY point width height mins maxs)
        ws (.stringWidth metrics text) hs (.getDescent metrics)]
    (.drawString offscreen ^String text (float (- xs (/ ws 2.0))) (float (+ ys hs)))))

(defn text-left [{:keys [^Graphics2D offscreen font mins maxs width height]} point text]
  (.setFont offscreen font)
  (let [metrics (.getFontMetrics offscreen)
        [xs ys] (scaleXY point width height mins maxs)
        hs (.getDescent metrics)]
    (.drawString offscreen ^String text (float xs) (float (+ ys hs)))))

(defn text-right [{:keys [^Graphics2D offscreen font mins maxs width height]} point text]
  (.setFont offscreen font)
  (let [metrics (.getFontMetrics offscreen)
        [xs ys] (scaleXY point width height mins maxs)
        ws (.stringWidth metrics text) hs (.getDescent metrics)]
    (.drawString offscreen ^String text (float (- xs ws)) (float (+ ys hs)))))