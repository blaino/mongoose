(ns mongoose.core
  (:import (java.awt Color Dimension) 
	   (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener))
  (:use clojure.contrib.import-static
        [clojure.contrib.seq-utils :only (includes?)]
        [clojure.contrib.math :only (abs)]
        clojure.set)
  (:gen-class :main true))


(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN)

; ----------------------------------------------------------
; functional model
; ----------------------------------------------------------
(def width 75)
(def height 50)
(def point-size 10)
(def turn-millis 75)
(def win-length 5)
(def dirs { VK_LEFT  [-1  0] 
            VK_RIGHT [ 1  0]
            VK_UP    [ 0 -1] 
	    VK_DOWN  [ 0  1]})

(defn add-points [& pts] 
  (vec (apply map + pts)))

(defn wrap [[x y]]
  (let [wrapped-x (rem (if (< x 0) (+ width x) x) width)
	wrapped-y (rem (if (< y 0) (+ height y) y) height)]
    [wrapped-x wrapped-y]))

(defn point-to-screen-rect [pt] 
  (map #(* point-size %) 
       [(pt 0) (pt 1) 1 1]))

(defn random-location []
  [(rand-int width) (rand-int height)])

(defn random-dir []
  (second (nth (vec dirs) (rand-int 3))))

(defn create-melons [n]
  {:locations (set (take n (repeatedly random-location)))
   :color (Color. 160 15 70)
   :type :melons})

(defn create-snake []
  {:body (list [1 1]) 
   :dir [1 0]
   :type :snake
   :color (Color. 15 160 70)})

(defn create-mongoose []
  {:body (list (random-location))  ; this doesn't need to be a list
   :dir [1 0]
   :attention (rand-int 20)
   :type :mongoose
   :color (Color. 50 50 50)})

(defmulti move (fn [object & _] (:type object)))

(defmethod move :snake [{:keys [body dir] :as snake} & grow]
  (assoc snake :body (cons (wrap (add-points (first body) dir))
			   (if grow body (butlast body)))))

(defmethod move :mongoose
  [{:keys [body dir attention] :as mongoose}]
  (if (> attention 0)
    (assoc mongoose
      :body (cons (wrap (add-points (first body) dir))
		  (butlast body))
      :attention (dec attention))
    (assoc mongoose
      :attention 10
      :dir (random-dir)
      )))

(defn eat-melon
  [{:keys [locations] :as melons} {[head] :body}]
  (assoc melons :locations
	 (union
	  (difference locations (set (list head)))
	  (set (list (random-location))))))

(defn turn [snake newdir] ; can be used for mongooses
  (assoc snake :dir newdir))

(defn win? [{body :body}]
  (>= (count body) win-length))

(defn head-overlaps-body? [{[head & body] :body}]
  (includes? body head))

(defn catches? [mongooses {snake :body}]
  (some #(includes? snake (first (:body (deref %)))) mongooses))

(defn lose? [snake mongooses]
  (or (head-overlaps-body? snake)
      (catches? mongooses snake)))

(defn eats? [{[snake-head] :body} melons]
  (contains? (:locations melons) snake-head))


; ----------------------------------------------------------
; mutable model
; ----------------------------------------------------------

(defn update-positions [snake melons mongooses]
  (dosync
   (doseq [mongoose mongooses] (alter mongoose move))
   (if (eats? @snake @melons)
     (do (alter melons eat-melon @snake)
	 (alter snake move :grow))
     (alter snake move)))
  nil)

(defn update-direction [snake newdir]
  (when newdir (dosync (alter snake turn newdir))))

(defn initialize
  []
  (def test-melons (ref nil))
  (def test-snake (ref nil))
  (def test-mongooses (map #(ref %)
			 (take 3 (repeatedly (fn [] nil))))))

(defn reset-game [snake melons mongooses]
  (dosync (ref-set melons (create-melons 5))
	  (ref-set snake (create-snake))
	  (doseq [mongoose mongooses]
	    (ref-set mongoose (create-mongoose))))
  nil)

;(initialize)
;(reset-game test-snake test-melons test-mongooses)


; ----------------------------------------------------------
; gui
; ----------------------------------------------------------

(defn fill-point [g pt color] 
  (let [[x y width height] (point-to-screen-rect pt)]
    (.setColor g color) 
    (.fillRect g x y width height)))

(defmulti paint (fn [g object & _] (:type object)))

(defmethod paint :melons [g {:keys [locations color]}]
  (doseq [location locations]
    (fill-point g location color)))

(defmethod paint :snake [g {:keys [body color]}] 
  (doseq [point body]
    (fill-point g point color)))

(defmethod paint :mongoose [g {:keys [body color]}]
  (doseq [point body]
    (fill-point g point color)))

(defn game-panel [frame snake melons mongooses]   
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g] 
      (proxy-super paintComponent g)
      (paint g @snake)
      (paint g @melons)
      (doseq [mongoose mongooses] (paint g @mongoose)))
    (actionPerformed [e] 
      (update-positions snake melons mongooses)   
      (when (lose? @snake mongooses)
	(reset-game snake melons mongooses)
	(JOptionPane/showMessageDialog frame "You lose!"))
      (when (win? @snake)
	(reset-game snake melons)
	(JOptionPane/showMessageDialog frame "You win!"))
      (.repaint this))
    (keyPressed [e] 
      (update-direction snake (dirs (.getKeyCode e))))
    (getPreferredSize [] 
      (Dimension. (* (inc width) point-size) 
		  (* (inc height) point-size)))
    (keyReleased [e])
    (keyTyped [e])))

(defn game []
  (let [snake (ref (create-snake)) 
	melons (ref (create-melons 10))
	mongooses (map #(ref %)
			 (take 5 (repeatedly create-mongoose)))
	frame (JFrame. "Snake")
	panel (game-panel frame snake melons mongooses) 
	timer (Timer. turn-millis panel)]
    (doto panel 
      (.setFocusable true)
      (.addKeyListener panel))
    (doto frame 
      (.add panel)
      (.pack)
      (.setVisible true))
    (.start timer) 
    [snake, melons, timer]))  

;; Display testing

(defn melons-panel [frame melons]  
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g] 
      (proxy-super paintComponent g)
      (paint g melons))
    (getPreferredSize [] 
      (Dimension. (* (inc width) point-size) 
		  (* (inc height) point-size)))))
    
(defn paint-melons [] 
  (let [melons (create-melons 10)
	frame (JFrame. "Snake")
	panel (melons-panel frame melons)]
    (doto panel 
      (.setFocusable true)
      (.addKeyListener panel))
    (doto frame 
      (.add panel)
      (.pack)
      (.setVisible true))))
