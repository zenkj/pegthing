(ns pegthing.core)

(def point-numbers (concat (range (int \a) (inc (int \z)))
                           (range (int \0) (inc (int \9)))
                           (range (int \A) (inc (int \Z)))
                           (map int [\@ \# \$ \%])))

; point string vector
(def point-strings (mapv (comp str char) point-numbers))

; point string to point number map
(def point-string-map (into {} (map vector point-strings (range))))

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
     

(def peg-string "+")
(def nopeg-string "-")
(def point-width 3)


(defn new-board
  [row-count]
  (when (and (>= row-count min-row-count)
           (<= row-count max-row-count))
    (let [point-count (inc (row-to-max-point (dec row-count)))
          board0      (into [] (repeat point-count
                                       {:pegged true
                                        :connections {}}))]
      )))


