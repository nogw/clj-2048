(ns haki.core
  (:require [lanterna.screen :as s])
  (:gen-class))

(defn get-time []
  (java.lang.System/nanoTime))

(def last-loop-time (atom 0))
(def target-fps (atom 20))
(defn optimal-time [] (double (/ 1000000000 @target-fps)))
(def fps (atom 0))
(def now (atom 0))
(def running (atom true))
(def scr (atom nil))

(defn insert-2 [board]
  (let [flat (into [] (flatten board))
        zero-indices (seq (keep-indexed #(when (zero? %2) %1) flat))]
    (cond
      (some #{2048} flat) (reset! running false)
      (empty? zero-indices) (reset! running false)
      :else (partition 4 (assoc flat (rand-nth zero-indices) 2)))))

(def board (atom (insert-2 (partition 4 (repeat 16 0)))))

(defn repeat-str
  [s n]
  (apply str (repeat n s)))

(defn spaces
  [n]
  (repeat-str \space n))

(defn center-str [s len]
  (let [slen (count (str s))
        lpad (int (/ (- len slen) 2))
        rpad (- len slen lpad)]
    (str (spaces lpad) s (spaces rpad))))

(defn get-color [n]
  (case n
    (0) {:fg :black :bg :white :styles #{:reverse}}
    (2 4) {:fg :black :bg :white :styles #{:bold}}
    (8 16 32) {:fg :black :bg :red :styles #{:bold}}
    (64 128 256 512 1024 2048) {:fg :black :bg :yellow :styles #{:bold}}))

(defn draw-square [col row text]
  (let [color (get-color text)]
    (s/put-string @scr (+ 1 (* 3 col)) (+ 1 (* 3 row)) "┌────┐" color)
    (s/put-string @scr (+ 1 (* 3 col)) (+ 2 (* 3 row)) (str "╎" (center-str text 4) "╎") color)
    (s/put-string @scr (+ 1 (* 3 col)) (+ 3 (* 3 row)) "└────┘" color)))

(defn draw-columns-and-rows [l]
  (loop [x (- (count l) 1)]
    (when (>= x 0)
      (loop [y (- (count (nth l x)) 1)]
        (when (>= y 0)
          (draw-square (* 2 y) x (nth (nth l x) y))
          (recur (- y 1))))
      (recur (- x 1)))))

(defn rotate [board]
  (map
   (fn [n]
     (map #(nth % n)
          board))
   [3 2 1 0]))

(defn remove-zeros [row]
  (remove zero? row))

(defn fill [row]
  (take 4 (concat row [0 0 0 0])))

(defn sum-up [acc x]
  (let [l (last acc)]
    (if (= x l)
      (conj (pop acc) (+ l x) 0)
      (conj acc x))))

(defn sum-pairs [v]
  (remove-zeros (reduce sum-up [] v)))

(defn slide [board]
  (map (comp
        fill
        sum-pairs
        remove-zeros) board))

(defn make-movement [move]
  (let [command (apply comp (assoc (into [] (repeat 5 rotate)) move slide))]
    (reset! board (insert-2 (command @board)))))

(defn get-input []
  (while @running
    (let [user-input (s/get-key @scr)]
      (cond
        (or (= \q user-input) (= :escape user-input)) (reset! running false)
        (or (= \a user-input) (= :left user-input)) (make-movement 4)
        (or (= \d user-input) (= :right user-input)) (make-movement 2)
        (or (= \w user-input) (= :up user-input)) (make-movement 3)
        (or (= \s user-input) (= :down user-input)) (make-movement 1)))
    (Thread/sleep 10)))

(defn draw []
  (draw-columns-and-rows @board))

(defn game-loop []
  (reset! last-loop-time (get-time))
  (while @running
    (s/clear @scr)
    (reset! now (get-time))
    (reset! last-loop-time @now)
    (reset! fps (inc @fps))
    (draw)
    (s/redraw @scr)
    (Thread/sleep (max 0 (double (/ (+ (optimal-time) (- @last-loop-time (get-time))) 1000000))))))

(defn -main []
  (reset! scr (s/get-screen :text))
  (.start (Thread. (fn [] (get-input))))
  (s/start @scr)

  (game-loop)
  (reset! running false)
  (s/stop @scr))