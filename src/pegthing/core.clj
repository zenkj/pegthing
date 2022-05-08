(ns pegthing.core
  (:gen-class))

(def max-row-count 11)
(def min-row-count 3)

; row number vector
(def point-to-row
  (apply vector (mapcat #(repeat (inc %) %)
                        (range max-row-count))))

(defn row-to-point [row-number]
  (if (zero? row-number)
    [0 0]
    (let [first (/ (* row-number (inc row-number)) 2)]
      [first (+ first row-number)])))

(defn row-to-min-point [row-number]
  (first (row-to-point row-number)))

(defn row-to-max-point [row-number]
  (second (row-to-point row-number)))

(defn row-to-points [row-number]
  (let [[min-point max-point] (row-to-point row-number)]
    (range min-point (inc max-point))))
     

(defn connect [board p1 over p2]
  (reduce (fn [prev-board [p1 p2]]
            (assoc-in prev-board [1 p1 :connections p2] over))
          board
          [[p1 p2] [p2 p1]]))

(defn init-right [board point-number]
  (let [over (inc point-number)
        right (inc over)
        row-number (point-to-row point-number)
        row-max (row-to-max-point row-number)]
    (if (and (not= point-number row-max)
             (not= over row-max))
      (connect board point-number over right)
      board)))

(defn init-down-left [board point-number]
  (let [row-number (point-to-row point-number)
        row-count  (first board)
        over       (+ point-number row-number 1)
        dest       (+ over row-number 2)]
    (if (> row-number (- row-count 3))
      board
      (connect board point-number over dest))))

(defn init-down-right [board point-number]
  (let [row-number (point-to-row point-number)
        row-count  (first board)
        over       (+ point-number row-number 2)
        dest       (+ over row-number 3)]
    (if (> row-number (- row-count 3))
      board
      (connect board point-number over dest))))

(defn new-board
  [row-count]
  (if (and (>= row-count min-row-count)
           (<= row-count max-row-count))
    (let [point-count (inc (row-to-max-point (dec row-count)))
          points      (into [] (repeat point-count
                                       {:pegged true
                                        :connections {}}))
          board       [row-count
                       (assoc-in points
                                 [(rand-int point-count) :pegged]
                                 false)]]
      (reduce (fn [board point-number]
                (reduce #(%2 %1 point-number)
                        board
                        [init-right
                         init-down-left
                         init-down-right]))
              board
              (range point-count)))
    (do (println "row count should be between" min-row-count "and" max-row-count)
        nil)))

(defn can-move [points from to]
  (let [over (get-in points [from :connections to])]
    (and (get-in points [from :pegged])
         (get-in points [over :pegged])
         (not (get-in points [to :pegged])))))

(defn movable? [points point-number]
  (some (fn [[to over]]
          (can-move points point-number to))
        (get-in points [point-number :connections])))

(defn game-over? [[row-count points]]
  (not (some #(movable? points %)
         (range (count points)))))

(defn move-peg [[row-count points :as board] from to]
  (let [over (get-in points [from :connections to])]
    (reduce (fn [board [p pegged]]
              (assoc-in board [1 p :pegged] pegged))
            board
            [[from false]
             [over false]
             [to   true]])))

;;;; for UI
(def point-numbers (concat (range (int \a) (inc (int \z)))
                           (range (int \0) (inc (int \9)))
                           (range (int \A) (inc (int \Z)))
                           (map int [\@ \# \$ \%])))

; point string vector
(def point-strings (mapv (comp str char) point-numbers))

; point string to point number map
(def point-string-map (into {} (map vector point-strings (range))))

(def peg-string "+")
(def nopeg-string "-")
(def point-width 3)

(defn str-point [point-number pegged]
  (str (point-strings point-number)
       (if pegged peg-string nopeg-string)
       " "))

(defn print-board [[row-count points]]
  (let [width (* row-count point-width)]
    (dotimes [i row-count]
      (let [row-point-count (inc i)
            row-point-width (* row-point-count point-width)
            left-padding (repeat (/ (- width row-point-width) 2) " ")
            point-string (map #(str-point % (:pegged (points %)))
                              (row-to-points i))]
        (println (apply str (concat left-padding point-string)))))))

(defn read-int [default]
  (let [line (read-line)]
    (try
      (Integer. (.trim line))
      (catch NumberFormatException e default))))

(defn read-chars []
  (let [line (read-line)]
    (filter #(> (count %) 0)
            (map #(.trim %)
                 (.split line "")))))

(defn read-points []
  (let [chars (read-chars)]
    (if (= (count chars) 2)
      (mapv #(point-string-map %) chars)
      [nil nil])))

(defn -main
  []
  (println "Enter the row count[5]: ")
  (let [row-count (read-int 5)]
    (loop [[row-count points :as board] (new-board row-count)]
      (if (game-over? board)
        (let [left (count (filter :pegged points))]
          (println "Game Over!" left "pegged points left."))
        (do (print-board board)
            (println "Move from where to where: ")
            (let [[from to] (read-points)]
              (if (and from (can-move points from to))
                (recur (move-peg board from to))
                (do
                  (println "Invalid input")
                  (recur board)))))))))
