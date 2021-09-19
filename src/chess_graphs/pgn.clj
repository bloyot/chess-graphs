(ns bloyot.chess-graphs.pgn
  (:require [bloyot.chess-graphs.core :as core]
            [clojure.string :as str])
  (:import (com.github.bhlangonijr.chesslib.pgn PgnHolder)
           (com.github.bhlangonijr.chesslib Board Square Piece)))

;; handle everything related to loading and processing pgn files here

(defn load-games!
  "loads the games from the pgn at the full file path given. Returns a
  vector of games."
  [pgn]
  (let [pgn-holder (new PgnHolder pgn)]
    (.loadPgn pgn-holder)
    (vec (.getGames pgn-holder))))

(defn board->map
  "Convert a java board object to a clojure friendly map of the pieces, indexed
  by rank and file"
  [board]
  (let [all-squares
        (for [rank core/ranks
              file core/files]
          {:rank rank
           :file file
           :piece (.getPiece board (Square/fromValue (str file rank)))})
        occ-squares (filter #(not= (:piece %) Piece/NONE) all-squares)]
    (reduce
     (fn [acc square]
       (let [side (first (str/split (.value (:piece square)) #"_"))
             piece (second (str/split (.value (:piece square)) #"_"))]
         (assoc-in acc
                   [(:file square) (:rank square)]
                   {:piece (keyword (str/lower-case piece))
                    :side (keyword (str/lower-case side))})))
     {} occ-squares)))
