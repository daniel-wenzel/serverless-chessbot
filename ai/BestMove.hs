{-# LANGUAGE OverloadedStrings #-}
module BestMove where

import Plot
import Board
import Piece
import Color

import Move
import CompMove

import PossibleMoves
import Settings
import Debug.Trace
import Strategy
import Utila

getBestMove :: Float -> Float -> Int -> Board -> Color -> CompMove
getBestMove alpha beta r board color = let all_moves = (getAllMovesSorted color board) in
                          if (length all_moves == 0) then (CompMove 0 0 0 0 (getBoardScore color board)) -- edge case when all figures were taken
                          else
                            let firstCMove =calculateMove r board color (head all_moves) alpha beta in
                            foldr (selectMove (calculateMove r board color) color alpha beta) firstCMove all_moves

--foldr :: (a -> b -> b) -> b -> [a] -> b
selectMove :: (Move -> Float -> Float -> CompMove) -> Color -> Float -> Float -> Move -> CompMove -> CompMove
selectMove makeMove color alpha beta move2 cMove1 =
                                  let comparator = if (color == White) then GT else LT in --set max or min
                                  let bestScore = if (getScore cMove1 `compare` alpha == comparator) then getScore cMove1 else alpha in
                                  --if cutoff && (getScore cMove1 `compare` alpha == comparator) then
                                  if cutoff && (getScore cMove1 `compare` beta == comparator) then --TODO: change
                                    --trace ("       cut "++show move2++ " alpha: "++show alpha++ show comparator)
                                    cMove1
                                  else
                                  let cMove2 = --trace ((show alpha) ++" "++ (show beta))
                                        makeMove move2 bestScore beta in --makeMove move2 (getScore cMove1) beta
                                  if (getScore cMove1 `compare` getScore cMove2 == comparator) then
                                    cMove1
                                  else
                                    cMove2

calculateMove :: Int -> Board -> Color -> Move -> Float -> Float -> CompMove
calculateMove r board color (Move a b x y) alpha beta = let newBoard = (getBoardAfterMove board (Move a b x y)) in
                                             let score = if (r == 0) then (getBoardScore color newBoard) + (getStrategicBoardScore newBoard color) + (scoreMoves color newBoard) - (scoreMoves (getOppositeColor color) newBoard)
                                                         else getScore (getBestMove (beta) (alpha) (r-1) newBoard (getOppositeColor color)) in
                                                         if (r>= traceDepth) then (trace ((show r) ++": "++show (CompMove a b x y score) ++ " new beta: "++(show alpha) ++ " new alpha: "++(show beta))) (CompMove a b x y score) else
                                                         (CompMove a b x y score)
