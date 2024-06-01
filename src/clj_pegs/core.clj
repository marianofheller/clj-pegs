(ns clj-pegs.core
  (:require [clojure.string :as str]))

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
      (let [should-move-up (= (+ count-in-row 1) curr-row)
            letter (get-letter (- (- total-count 1) curr-count))]
        (recur
         (conj board {:row     (dec curr-row)
                      :pos     (dec (- curr-row count-in-row))
                      ;:id (random-uuid)
                      :has-peg (not= letter "e")
                      :letter  letter})
         (+ curr-count 1)
         (if should-move-up 0 (+ count-in-row 1))
         (if should-move-up (- curr-row 1) curr-row))))))

(defn pad-left [s n] (apply str (concat (take n (repeat " ")) [s])))

(defn sort-board [board] (->> board
                              (sort-by :letter)
                              (sort-by :row)
                              (sort-by :pos)))

(def prettify-board
  (letfn [(peg-to-str [peg] (if peg "0" "-"))
          (cell-to-str [cell] (#(str (:letter %) (peg-to-str (:has-peg %)) " ") cell))
          (cells-to-str [cells] (apply str (map cell-to-str cells)))
          (line-to-str [[rn cells]] (pad-left (cells-to-str cells) (- max-row-count rn)))
          (prettify-line [line] (str (line-to-str line) "\n"))]
    (comp (partial apply str) (partial map prettify-line) (partial group-by :row) (partial sort-board))))

(defn dec2 [a] (- a 2))
(defn inc2  [a] (+ a 2))

(defn get-possible-dsts [board s-letter]
  (let [src (first (filter #(= (:letter %) s-letter) board))
        src-idx (:pos src)
        src-row (:row src)
        possible-positions [{:pos (dec2 src-idx) :row (dec2 src-row)},
                            {:pos (dec2 src-idx) :row src-row},
                            {:pos (dec2 src-idx) :row (inc2 src-row)},
                            {:pos (inc2 src-idx) :row (dec2 src-row)},
                            {:pos (inc2 src-idx) :row src-row},
                            {:pos (inc2 src-idx) :row (inc2 src-row)},
                            {:pos src-idx :row (dec2 src-row)},
                            {:pos src-idx :row (inc2 src-row)}]
        is-valid-dst  (fn [cell] (> (count (filter
                                            #(and (= (:pos %1) (:pos cell)) (= (:row %1) (:row cell)))
                                            possible-positions))
                                    0))
        possible-dsts (filter is-valid-dst board)]
    possible-dsts))

(defn game []
  (loop [board (generate-board)]
    (println (prettify-board board))
    (println "Enter peg to move and target place: ")
    (let [[s, d] (str/split (read-line) #" ")]
      (if (= s "quit") 0 (let [possible-dsts (get-possible-dsts board s)
                               dst (nth (filter #(= (:letter %1) d) possible-dsts) 0)
                               src (nth (filter #(= (:letter %1) s) board) 0)
                               delta-y (quot (- (:row dst) (:row src)) 2)
                               delta-x (quot (- (:pos dst) (:pos src)) 2)
                               middle (nth (filter
                                            #(and (= (:pos %1) (+ (:pos src) delta-x)) (= (:row %1) (+ (:row src) delta-y)))
                                            board) 0)]
                           (cond
                             (not (:has-peg src)) (do (println "Src tile should have a peg") (recur board))
                             (:has-peg dst) (do (println "Dst tile should not have a peg") (recur board))
                             (not (:has-peg middle)) (do (println "Jumped tile should have a peg") (recur board))
                             :else (let [updated-dst (assoc dst :has-peg true)
                                         updated-src (assoc src :has-peg false)
                                         updated-middle (assoc middle :has-peg false)
                                         updated-board (conj (filter #(and (not (= (:letter %1) (:letter src)))
                                                                           (not (= (:letter %1) (:letter dst)))
                                                                           (not (= (:letter %1) (:letter middle))))
                                                                     board)
                                                             updated-dst updated-src updated-middle)]
                                     (recur updated-board))))))))

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
