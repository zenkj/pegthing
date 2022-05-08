(ns pegthing.core_v2
  (:gen-class))

;; core function -- 53m
;; {:row-count row-count :points [{:pegged true :connections {from-point to-point}}]}

(def min-row-count 3)
(def max-row-count 11)

(def point->row (into [] (mapcat #(repeat (inc %) %)
                                (range max-row-count))))

(defn row->points [row-number]
  (let [min-point (/ (* (inc row-number) row-number) 2)
        max-point (+ min-point row-number)]
    [min-point max-point]))

(defn connect [board from over to]
  (reduce (fn [prev-board [p1 p2]]
            (assoc-in prev-board [:points p1 :connections p2] over))
          board
          [[from to] [to from]]))

(defn connect-right [board point-number]
  (let [row-number (point->row point-number)
        [_ row-max] (row->points row-number)
        over (inc point-number)
        dest (inc over)]
    (if (or (= point-number row-max)
            (= over row-max))
      board
      (connect board point-number over dest))))

(defn connect-down-left [{:keys [row-count points] :as board}
                         point-number]
  (let [row-number (point->row point-number)
        over       (+ point-number row-number 1)
        dest       (+ over row-number 2)]
    (if (> row-number (- row-count 3))
      board
      (connect board point-number over dest))))

(defn connect-down-right [{:keys [row-count points] :as board}
                          point-number]
  (let [row-number (point->row point-number)
        over       (+ point-number row-number 2)
        dest       (+ over row-number 3)]
    (if (> row-number (- row-count 3))
      board
      (connect board point-number over dest))))

(defn create-board [row-count]
  (let [point-count (/ (* (inc row-count) row-count) 2)
        points      (into [] (repeat point-count {:pegged true :connections {}}))
        board       {:row-count row-count :points (assoc-in points [(rand-int point-count) :pegged] false)}]
    (reduce (fn [prev-board point-number]
              (reduce #(%2 %1 point-number)
                      prev-board
                      [connect-right
                       connect-down-left
                       connect-down-right]))
            board
            (range point-count))))

;; render function -- 37m
(def peg-string "+")
(def nopeg-string "-")
(def point-width 3)

(def point->string (mapv (comp str char)
                         (concat (range (int \a) (inc (int \z)))
                                 (range (int \A) (inc (int \Z)))
                                 (range (int \0) (inc (int \9)))
                                 (map int [\@ \# \$ \%]))))

(def string->point (into {} (map vector point->string (range))))

(defn pegpoint->string [points point-number]
  (str (point->string point-number)
       (if (get-in points [point-number :pegged])
         peg-string
         nopeg-string)
       " "))

(defn padding [row-count row-number]
  (let [width (* row-count point-width)
        row-width (* (inc row-number) point-width)
        padding-width (/ (- width row-width) 2)]
    (apply str (repeat padding-width " "))))

(defn print-board [{:keys [row-count points]}]
  (dotimes [row-number row-count]
    (let [[min-point max-point] (row->points row-number)
          ps (map #(pegpoint->string points %)
                  (range min-point (inc max-point)))
          line (apply (partial str (padding row-count row-number)) ps)]
      (println line))))

;; move function --
(defn over-point [points from to]
  (get-in points [from :connections to]))

(defn pegged? [points point-number]
  (get-in points [point-number :pegged]))

(defn can-move [{:keys [points]} from to]
  (let [over (over-point points from to)]
    (and (pegged? points from)
         (not (pegged? points to))
         (pegged? points over))))

(defn movable? [board point-number]
  (some (fn [[to over]] (can-move board point-number to))
        (get-in board [:points point-number :connections])))

(defn gameover? [board]
  (not (some #(movable? board %) (range (count (board :points))))))

(defn move [board from to]
  (let [over (get-in board [:points from :connections to])]
    (reduce (fn [prev-board [point-number pegged]]
              (assoc-in prev-board [:points point-number :pegged] pegged))
            board
            [[from false]
             [over false]
             [to   true]])))

;; main function
(defn read-where-to-where []
  (map #(get string->point %)
       (filter #(> (count %) 0)
               (map #(.trim (str %)) (read-line)))))

(defn read-int [default]
  (try
    (Integer. (.trim (read-line)))
    (catch NumberFormatException e 5)))

(defn -main [& args]
  (println "Please input row count(3-11)[5]:")
  (let [row-count (read-int 5)]
    (loop [board (create-board row-count)]
      (print-board board)
      (if (gameover? board)
        (println "Game Over!" (count (filter #(:pegged %) (:points board))) "pegs left")
        (do
          (println "Please input where to where:")
          (let [w2w (read-where-to-where)]
            (if (and (= (count w2w) 2)
                     (can-move board (first w2w) (second w2w)))
              (recur (move board (first w2w) (second w2w)))
              (do
                (println "invalid input!")
                (recur board)))))))))
