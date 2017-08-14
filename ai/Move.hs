{-# LANGUAGE OverloadedStrings #-}
module Move where

import Data.Char
import Board
import Piece
import Color
import Plot


data Move = Move Int Int Int Int
instance Eq Move where
    (==) (Move w x y z) (Move a b c d) = (a==w) && (x==b) && (y==c) && (z==d)



instance Show Move where
    show (Move x1 y1 x2 y2) = [chr (97+x1)]++show (y1+1)++" "++[chr (97+x2)]++show (y2+1)


moveComperator :: Board -> Move -> Move -> Ordering
moveComperator b (Move x_old1 y_old1 x_new1 y_new1) (Move x_old2 y_old2 x_new2 y_new2) = ((getFigureHitScoreAt x_new1 y_new1 b)) `compare` ((getFigureHitScoreAt x_new2 y_new2 b))

getBoardAfterMove :: Board -> Move -> Board
getBoardAfterMove board (Move x1 y1 x2 y2) =let newBoard = setPlot (setPlot board (changePlotCoords x2 y2 (getPlotAt x1 y1 board))) (Plot x1 y1 None Empty)
                                                newPlot = getPlotAt x2 y2 newBoard in
                                            if  y2 == 0 && getFigureAtPlot newPlot == Pawn && getColorOfPlot newPlot == Black -- Promotion Black
                                                then setPlot newBoard (Plot x2 y2 Black Queen)
                                            else if  y2 == 7 && getFigureAtPlot newPlot == Pawn && getColorOfPlot newPlot == White -- Promotion White
                                                then setPlot newBoard (Plot x2 y2 White Queen)
                                                else newBoard
