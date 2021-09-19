(ns bloyot.chess-graphs.threats
  (:require [bloyot.chess-graphs.core :as core]))

(defn threatened-squares
  "Return the threated squares based on the movement pattern and the
  number of iterations. Accounts for squares blocked by another piece."
  [board starting-file starting-rank mvmt n]
  (loop [remaining n
         file starting-file
         rank starting-rank
         result []]
    (let [square (core/get-square board file rank mvmt)
          updated-result (if square (conj result square) result)]
      ;; if there's more squares to check, and the square we just considered
      ;; does not have a piece in it (i.e blocks), recur
      (if (and square (< 0 (dec remaining)) (not (:piece square)))
        (recur (dec remaining) (:file square) (:rank square) updated-result)
        updated-result))))


(defn pawn-threats
  "return the ranks and files threated by this pawn"
  [board file rank side]
  (let [forward-left (if (= side :white) [1 1] [1 -1])
        forward-right (if (= side :white) [-1 1] [-1 -1])]
    (vec (concat
          (threatened-squares board file rank forward-left 1)
          (threatened-squares board file rank forward-right 1)))))

(defn knight-threats
  "return the ranks and files threated by this knight"
  [board file rank]
  (vec (concat
        (threatened-squares board file rank [1 2] 1)
        (threatened-squares board file rank [1 -2] 1)
        (threatened-squares board file rank [-1 2] 1)
        (threatened-squares board file rank [-1 -2] 1))))

(defn bishop-threats
  "return the ranks and files threated by this bishop"
  [board file rank]
  (vec (concat
        (threatened-squares board file rank [1 1] 7)
        (threatened-squares board file rank [1 -1] 7)
        (threatened-squares board file rank [-1 1] 7)
        (threatened-squares board file rank [-1 -1] 7))))

(defn rook-threats
  "return the ranks and files threated by this rook"
  [board file rank]
  (vec (concat
        (threatened-squares board file rank [1 0] 7)
        (threatened-squares board file rank [0 1] 7)
        (threatened-squares board file rank [-1 0] 7)
        (threatened-squares board file rank [0 -1] 7))))

(defn queen-threats
  "return the ranks and files threated by this rook"
  [board file rank]
  (vec (concat (rook-threats board file rank)
               (bishop-threats board file rank))))

(defn king-threats
  "return the ranks and files threated by this rook"
  [board file rank]
  (vec (concat
        (threatened-squares board file rank [1 0] 1)
        (threatened-squares board file rank [1 1] 1)
        (threatened-squares board file rank [1 -1] 1)
        (threatened-squares board file rank [0 1] 1)
        (threatened-squares board file rank [0 -1] 1)
        (threatened-squares board file rank [-1 0] 1)
        (threatened-squares board file rank [-1 1] 1)
        (threatened-squares board file rank [-1 -1] 1))))

(defn square->threats
  "For a given square return a vector of squares it's piece threatens, in the
  form of {:rank A :file 1}"
  [board {:keys [piece file rank side]}]
  (case piece
    :pawn (pawn-threats board file rank side)
    :knight (knight-threats board file rank)
    :bishop (bishop-threats board file rank)
    :rook (rook-threats board file rank)
    :queen (queen-threats board file rank)
    :king (king-threats board file rank)))

