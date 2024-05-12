(ns clj-pegs.core)

(def max-row-count 5)

(def total-count (/ (* max-row-count (+ max-row-count 1)) 2))

(def get-letter (comp str char #(+ 97 %)))

(defn generate-board
  []
  (loop [board []
         curr-count 0
         count-in-row 0
         curr-row max-row-count]
    (if (= curr-row 0)
      (sort-by :letter board)
      (let [should-move-up (= (+ count-in-row 1) curr-row)]
        (recur
          (conj board {
                       :row    curr-row
                       :pos    (- curr-row count-in-row)
                       :peg    true
                       :letter (get-letter (- (- total-count 1) curr-count))})
          (+ curr-count 1)
          (if should-move-up 0 (+ count-in-row 1))
          (if should-move-up (- curr-row 1) curr-row))))))

(defn pad-left [s n] (apply str (concat (take n (repeat " ")) [s])))

(def print-board
  (letfn [(peg-to-str [peg] (if peg "0" "-"))
          (cell-to-str [cell] (#(str (:letter %) (peg-to-str (:peg %)) " ") cell))
          (cells-to-str [cells] (apply str (map cell-to-str cells)))
          (line-to-str [[rn cells]] (pad-left (cells-to-str cells) (- max-row-count rn)))
          (print-line [line] (println (line-to-str line)))]
    (comp (partial map print-line) (partial group-by :row))))


;Creating a new board
;Returning a board with the result of the player’s move
;Representing a board textually
;Handling user interaction

;    a0
;   b0 c0
;  d0 e0 f0
; g0 h- i0 j0
;k0 l- m0 n0 o0
;
;a0             1
;b0 c0          3
;d0 e0 f0       6
;g0 h- i0 j0    10
;k0 l- m0 n0 o0 15