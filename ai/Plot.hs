{-# LANGUAGE OverloadedStrings #-}
module Plot where

import Color
import Piece


data Plot = Plot Int Int Color Piece

instance Show Plot where
    show (Plot x y Black King) = "("++show x++","++show y++": k)"
    show (Plot x y White King) = "("++show x++","++show y++": K)"
    show (Plot x y Black Pawn) = "("++show x++","++show y++": p)"
    show (Plot x y White Pawn) = "("++show x++","++show y++": P)"
    show (Plot x y Black Rook) = "("++show x++","++show y++": r)"
    show (Plot x y White Rook) = "("++show x++","++show y++": R)"
    show (Plot x y Black Bishop) = "("++show x++","++show y++": b)"
    show (Plot x y White Bishop) = "("++show x++","++show y++": B)"
    show (Plot x y Black Queen) = "("++show x++","++show y++": q)"
    show (Plot x y White Queen) = "("++show x++","++show y++": Q)"
    show (Plot x y Black Knight) = "("++show x++","++show y++": n)"
    show (Plot x y White Knight) = "("++show x++","++show y++": N)"
    show (Plot x y None _) = "("++show x++","++show y++": _)"

getPlotValue :: Plot -> Float
getPlotValue (Plot x y figure_color piece) = getPieceValue piece

--Sets the coordinates of a plot
changePlotCoords :: Int -> Int -> Plot -> Plot
changePlotCoords x y (Plot _ _ c f) = Plot x y c f

getColorOfPlot :: Plot -> Color
getColorOfPlot (Plot x y c f) = c

getFigureAtPlot :: Plot -> Piece
getFigureAtPlot (Plot x y c f) = f

-- is the given plot empty?, color == none is empty
isEmpty :: Plot -> Bool
isEmpty (Plot _ _ c _)  = if c == None then True else False
