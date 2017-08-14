{-# LANGUAGE OverloadedStrings #-}
module Strategy where

import Board
import Plot
import Piece
import Color
import Debug.Trace
import PossibleMoves
import Move
getStrategicBoardScore :: Board -> Color -> Float
getStrategicBoardScore b c = getStrategicBoardScore' b c b


getStrategicBoardScore' :: Board -> Color -> [Plot] -> Float
getStrategicBoardScore' board _ [] = 0
getStrategicBoardScore' board player_color ((Plot x y figure_color piece):xs) = (getStrategicBoardScore' board player_color xs) + if (player_color == figure_color) then
                                                                                            (getStrategicPieceScore board (Plot x y figure_color piece))
                                                                                         else (getStrategicPieceScore board (Plot x y figure_color piece)) * (-1)

getStrategicPieceScore :: Board -> Plot -> Float
getStrategicPieceScore board (Plot x y color King) =
  if (y == 0 || y == 7) then 0.1 else 0
getStrategicPieceScore board (Plot x y color Pawn) = ratePawn board (Plot x y color Pawn)
getStrategicPieceScore board (Plot x y color Knight) = if (y == 0 || y == 7) then -0.5 else 0
getStrategicPieceScore board (Plot x y color Bishop) = if (y == 0 || y == 7) then -0.5 else 0
getStrategicPieceScore board (Plot x y color f) = 0
--getStrategicPieceScore board (Plot x y color Rook) = if (y == 0 || y == 7) -0.1 else 0

ratePawn :: Board -> Plot -> Float
ratePawn b (Plot x y White _) =
  let centerScore = if (y >= 3 && y <= 4 && x >= 3 && x <= 4) then 0.8
                                                  else if (y >= 2 && y <= 5 && x >= 2 && x <= 5) then 0.3 else 0 in
  let kingWingScore = if (x > 4 || y < 3) then 0.15 else 0 in
  let protectionScore = pawnProtectionScore ((getValidPlot (x+1) (y+1) b) ++ (getValidPlot (x-1) (y+1) b)) in
  centerScore + kingWingScore + protectionScore
ratePawn b (Plot x y Black _) =
  let centerScore = if (y >= 3 && y <= 4 && x >= 3 && x <= 4) then 0.8
                                                  else if (y >= 2 && y <= 5 && x >= 2 && x <= 5) then 0.3 else 0 in
  let kingWingScore = if (x > 4 || y > 4) then 0.15 else 0 in
  let protectionScore = pawnProtectionScore ((getValidPlot (x+1) (y-1) b) ++ (getValidPlot (x-1) (y-1) b)) in
  centerScore + kingWingScore + protectionScore


pawnProtectionScore :: [Plot] -> Float
pawnProtectionScore [] = 0
pawnProtectionScore ((Plot x y _ Empty): xs) = 0 + pawnProtectionScore xs
pawnProtectionScore ((Plot x y _ _): xs) = 0.1 + pawnProtectionScore xs

scoreMoves :: Color -> Board -> Float
scoreMoves c b = scoreMoves' b (getAllMoves c b)

scoreMoves' :: Board -> [Move] -> Float
scoreMoves' b [] = 0
scoreMoves' b (m:xs) = (scoreMove b m) + scoreMoves' b xs

scoreMove :: Board -> Move -> Float
scoreMove b (Move x_from y_from x_to y_to) = scoreMove' (getFigureAt x_from y_from b) (getFigureAt x_to y_to b)
--Which piece can move to which plot?
scoreMove' :: Piece -> Piece -> Float
scoreMove' Pawn Queen = 0.05
scoreMove' Pawn King = 0.05
scoreMove' Pawn Bishop = 0.05
scoreMove' Pawn Knight = 0.05
scoreMove' Pawn Rook = 0.05
scoreMove' Queen King = 0.1
scoreMove' Queen Empty = 0.01
scoreMove' King _ = 0.005
scoreMove' Rook King = 0.1
scoreMove' Rook Queen = 0.1
scoreMove' Rook Bishop = 0.075
scoreMove' Rook Knight = 0.075
scoreMove' Rook Pawn = 0.01
scoreMove' Rook _ = 0.075
scoreMove' Knight Empty = 0.15
scoreMove' Knight Knight = 0
scoreMove' Knight _ = 0.3
scoreMove' Bishop Bishop = 0
scoreMove' Bishop Queen = 0.1
scoreMove' Bishop King = 0.1
scoreMove' Bishop Empty = 0.05
scoreMove' Bishop Rook = 0.1
scoreMove' _ _ = 0
