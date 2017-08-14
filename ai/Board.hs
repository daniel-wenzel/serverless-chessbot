{-# LANGUAGE OverloadedStrings #-}
module Board where


import Color
import Piece
import Plot
import Settings
import Data.Char
import Debug.Trace

type Board = [Plot]



emptyBoard :: Board
emptyBoard = emptyBoard' 63

emptyBoard' :: Int -> Board
emptyBoard' 0 = [Plot 0 0 None Empty]
emptyBoard' x = [Plot (x `mod` 8) (quot x 8)  None Empty] ++ emptyBoard' (x-1)


addPlot :: Int -> Char -> Plot
addPlot n c = Plot x y color piece
        where x = 7-(n `mod` 8)
              y = (quot n 8)
              color = if (isUpper c) then White else Black
              piece = case (toUpper c) of
                'P' -> Pawn
                'N' -> Knight
                'B' -> Bishop
                'R' -> Rook
                'Q' -> Queen
                'K' -> King

loadFen :: Int -> Board -> String -> Board
loadFen n b ('/':xs) = loadFen n b (xs)
loadFen n b [] = b
loadFen n b (x:xs) = if(x `elem` ['1'..'9']) then loadFen (n- digitToInt x) b xs
        else loadFen (n-1) b' xs where
             b' = setPlot b (addPlot n x)



getFenBoard :: String -> Board
getFenBoard s = loadFen 63 emptyBoard s

--Returns a new Board with the plot changed (it will change the plot on the coordinates to the argument values)
setPlot :: Board -> Plot -> Board
setPlot ((Plot x1 y1 c1 f1): xs) (Plot x2 y2 c2 f2) =
    if (x1 == x2 && y1 == y2) then
        [(Plot x2 y2 c2 f2)] ++ xs--setPlot xs (Plot x2 y2 c2 f2)
    else
        [(Plot x1 y1 c1 f1)] ++ setPlot xs (Plot x2 y2 c2 f2)



getFigureAt :: Int -> Int -> Board -> Piece
getFigureAt x y b = getFigureAtPlot (getPlotAt x y b)

-- Returns the plot at the argument coordinates
getPlotAt :: Int -> Int -> Board -> Plot
getPlotAt x y ((Plot x1 y1 c d):xs) = if (x == x1 && y == y1) then (Plot x1 y1 c d) else getPlotAt x y xs
getPlotAt x y _ = error ("Invalid plot coordinates: "++show x ++" "++show y)


-- Returns a plot at the coordinates, if they are on the board
getValidPlot :: Int -> Int -> Board -> [Plot]
getValidPlot x y b= if (x < 0 || y < 0 || x > 7 || y > 7) then [] else [getPlotAt x y b]

--Returns the plot on which the King with that color currently resides
getKingPlot :: Color -> Board -> Plot
getKingPlot c ((Plot x y plot_color King):xs) = if (c == plot_color)  then (Plot x y plot_color King) else getKingPlot c xs
getKingPlot c (_:xs) = getKingPlot c xs

-- is there an opponent's piece on the plot?
getPlotsWithOppositeColor :: Color -> [Plot] -> [Plot]
getPlotsWithOppositeColor c [] = []
getPlotsWithOppositeColor c ((Plot x y c2 p):xs) =  if (c2 == (getOppositeColor c)) then
        [(Plot x y c2 p)] ++ getPlotsWithOppositeColor c xs
      else getPlotsWithOppositeColor c xs

-- Returns all plots within a list of plots that are not hold by the argument color
getPlotsWithNotSameColor :: Color -> [Plot] -> [Plot]
getPlotsWithNotSameColor c [] = []
getPlotsWithNotSameColor c ((Plot x y c2 p):xs) =
    if (c /= c2) then
        [(Plot x y c2 p)] ++ getPlotsWithNotSameColor c xs
    else getPlotsWithNotSameColor c xs


getFigureHitScoreAt :: Int -> Int -> Board -> Float
getFigureHitScoreAt x y b = let f = (getFigureAt x y b) in
                        getPieceValue f


getBoardScore :: Color -> [Plot] -> Float
getBoardScore _ [] = 0
getBoardScore player_color ((Plot x y figure_color piece):xs) = (getBoardScore player_color xs) + if (player_color == figure_color) then
                                                                                          (getPieceValue piece)
                                                                                       else (getPieceValue piece) * (-1)
