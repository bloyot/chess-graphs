(ns chess-graphs.core
  (:require [chess-graphs.pgn :as pgn]
            [chess-graphs.threats :as threats])
  (:import (com.github.bhlangonijr.chesslib Board)))

(defn board->square-vec
  "given the board map, return a vector of squares, where each square has the
  rank, file, piece, and side."
  [board]
  (let [map-file (fn [f [r rest]] (merge {:file f :rank r} rest))
        map-rank (fn [[f rs]] (map #(map-file f %) rs))]
    (vec (mapcat map-rank board))))

(defn chess-map
  "Create the a map with the pressure and control value for each game in
  the given pgn file."
  [board]
  ;; todo load from games
  (let [occ-squares (board->square-vec board)
        white-squares (filter #(= :white (:side %)) occ-squares)
        black-squares (filter #(= :black (:side %)) occ-squares)
        white-threats (mapcat #(threats/square->threats board %) white-squares)
        black-threats (mapcat #(threats/square->threats board %) black-squares)
        combined (concat
                  (map #(assoc % :side :white) white-threats)
                  (map #(assoc % :side :black) black-threats))]
    (loop [squares combined
           result {}]
      (if (seq squares)
        (let [{:keys [rank file side]} (first squares)]
          (recur (rest squares)
                 (update-in result [file rank side] #(if % (inc %) 1))))
        result))))

;; example:
;; (analyze-game (first (pgn/load-games! "resources/sample_pgn.pgn")))
(defn analyze-game
  "given a particular game, create a the chess map for each half move
  state."
  [game]
  (let [curr-state (new Board)
        move-list (vec (.getHalfMoves game))]
    ;; loop through each move, applying it to the board and storing it in
    ;; the result vector
    (loop [moves move-list
           result []]
      (if (seq moves)
        (do
          (.doMove curr-state (first moves))
          (let [curr-chess-map (chess-map (pgn/board->map curr-state))]
            (recur (rest moves) (conj result curr-chess-map))))
        result))))

