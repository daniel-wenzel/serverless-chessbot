{-# LANGUAGE OverloadedStrings #-}
module PossibleMoves where

import Plot
import Board
import Piece
import Color

import Move

import Data.List (sortBy)
--Returns a list of plots you can go to from a certain plot. Doesnt care about check. Can (and often will be) []
getMoves :: Plot -> [Plot] -> [Move]
getMoves p [] = []
getMoves (Plot x1 y1 c p) ((Plot x2 y2 _ _):xs) = [(Move x1 y1 x2 y2)] ++ getMoves (Plot x1 y1 c p) xs

-- Checks where you can go to from a certain plot
canMoveToPlots :: Plot -> Board -> [Plot]
canMoveToPlots (Plot x y color King) board = getPlotsWithNotSameColor color (getNeighbours (Plot x y color King) board)
canMoveToPlots (Plot x y color Pawn) board = getPlotsWithNotSameColor color (getPawnPlots (Plot x y color Pawn) board)
canMoveToPlots (Plot x y color Rook) board = (getMovesInDir (Plot x y color Rook) (Dir 1 0) board) ++ (getMovesInDir (Plot x y color Rook) (Dir (-1) 0) board)
                                            ++ (getMovesInDir (Plot x y color Rook) (Dir 0 (-1)) board) ++ (getMovesInDir (Plot x y color Rook) (Dir 0 1) board)
canMoveToPlots (Plot x y color Bishop) board = (getMovesInDir (Plot x y color Bishop) (Dir 1 1) board) ++ (getMovesInDir (Plot x y color Bishop) (Dir (-1) 1) board)
                                            ++ (getMovesInDir (Plot x y color Bishop) (Dir 1 (-1)) board) ++ (getMovesInDir (Plot x y color Bishop) (Dir (-1) (-1)) board)
canMoveToPlots (Plot x y color Queen) board = canMoveToPlots (Plot x y color Bishop) board ++ canMoveToPlots (Plot x y color Rook) board
canMoveToPlots (Plot x y color Knight) board = getPlotsWithNotSameColor color (getKnightPlots (Plot x y color Knight) board)
canMoveToPlots (Plot x y color _) board = error ("No idea how to move figure at "++show x ++" "++show y)

-- gets the neighbours of one plot (relevant for King)
getNeighbours :: Plot -> Board -> [Plot]
getNeighbours (Plot x y _ _) xs = getValidPlot (x-1) (y-1) xs ++ getValidPlot (x) (y-1) xs ++ getValidPlot (x+1) (y-1) xs ++
                                 getValidPlot (x-1) (y) xs ++               getValidPlot (x+1) (y) xs ++
                                 getValidPlot (x-1) (y+1) xs ++ getValidPlot (x) (y+1) xs ++ getValidPlot (x+1) (y+1) xs

-- returns all posibble moves for the Knight on the specified Plot
getKnightPlots :: Plot -> Board -> [Plot]
getKnightPlots (Plot x y _ _) xs = filter (\(Plot xP yP _ _) -> (abs (xP-x)) == 2 && (abs (yP-y)) == 1 || (abs (xP-x)) == 1 && (abs (yP-y)) == 2)  xs

-- returns all posibble moves for the Pawn on the specified Plot
getPawnPlots :: Plot -> Board -> [Plot]
getPawnPlots (Plot x y c p) b = (if c == White
                                                then getWhitePawnPlots (Plot x y c p) b
                                                else getBlackPawnPlots (Plot x y c p) b)
--  returns all posibble moves for the black  Pawn on the specified Plot
getBlackPawnPlots :: Plot -> Board -> [Plot]
getBlackPawnPlots (Plot x y c _) b = let p = getValidPlot x (y-1) b in --laufen
                                        filter isEmpty (if (y == 6) && (isEmpty (head p)) then getValidPlot x (y-2) b ++ p
                                                else p) -- laufen
                                                 ++  getPlotsWithOppositeColor c (getValidPlot (x-1) (y-1) b ++ getValidPlot (x+1) (y-1) b)

--  returns all posibble moves for the white  Pawn on the specified Plot
getWhitePawnPlots :: Plot -> Board -> [Plot]
getWhitePawnPlots (Plot x y c _) b = let p = getValidPlot x (y+1) b in --laufen
                                        filter isEmpty (if (y == 1) && (isEmpty (head p)) then getValidPlot x (y+2) b ++ p
                                                else p) -- laufen
                                                 ++  getPlotsWithOppositeColor c (getValidPlot (x+1) (y+1) b ++ getValidPlot (x-1) (y+1) b)

--getPlotInDir :: Plot -> Dir -> Board -> [Plot]
--getPlotInDir (Plot x y plot_c f):xs (Dir xdir ydir) b = getValidPlot x+xdir y+ydir b
data Dir = Dir  Int Int
getMovesInDir :: Plot -> Dir -> Board -> [Plot]
getMovesInDir (Plot x y c f) (Dir xdir ydir) b = getMovesInDir' c (getValidPlot (x+xdir) (y+ydir) b) (Dir xdir ydir) b

getMovesInDir' :: Color -> [Plot] -> Dir -> Board -> [Plot]
getMovesInDir' _ [] _ _ = []
getMovesInDir' col ((Plot x y plot_c f):xs) (Dir xdir ydir) b =
    if (col == plot_c) then
        []
    else if (plot_c == None) then
        [(Plot x y plot_c f)] ++ getMovesInDir' col (getValidPlot (x+xdir) (y+ydir) b) (Dir xdir ydir) b
         else
            [(Plot x y plot_c f)]

--This includes moves that leave the King attacked.
getAllMoves' :: Color -> Board -> Board -> [Move]
getAllMoves' c board [] = []
getAllMoves' c board (p: xs) = getPlotMoves p c board ++ getAllMoves' c board xs

--Returns all moves that are possible from a given plot. Returns an empty list if there is not a figure with the color on the plot
getPlotMoves :: Plot -> Color -> Board-> [Move]
getPlotMoves (Plot x y plot_color f) color b =
    if (color == plot_color) then
        getMoves (Plot x y plot_color f) (canMoveToPlots (Plot x y plot_color f) b)
    else []

--Returns all moves. This includes moves that leave the King attacked.
getAllMoves :: Color -> Board -> [Move]
getAllMoves c b = getAllMoves' c b b

getAllMovesSorted :: Color -> Board -> [Move]
getAllMovesSorted c b = sortBy (moveComperator b) (getAllMoves c b)
