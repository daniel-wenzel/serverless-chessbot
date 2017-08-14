    {-# LANGUAGE OverloadedStrings #-}
module ChessBot where

import Data.Char

import Color
import Piece
import Plot
import Board
import Settings

import Fen
import Move
import CompMove

import PossibleMoves
import AllowedMoves
import BestMove

botListAllMoves :: String -> String -> [String]
botListAllMoves board color = if color == "w" then map show (getFenMoves board White) else map show (getFenMoves board Black)

-- returns one valid move in the format [source target]
-- parameter 1: first part of FEN String
-- parameter 2: "w"/"b"
botFindMove :: String -> String -> String
botFindMove board color = let b = (getFenBoard board) in if color == "w" then show (botFindMove' White b) else show (botFindMove' Black b)

botFindMove' :: Color -> Board -> Move
botFindMove' color board = let matemoves = (getCheckMateMoves color board) in if length matemoves > 0
                                                                            then head matemoves
                                                                            else compMoveToMove (getBestMove (alpha0 color) (beta0 color) depth board color)

getFenMoves :: String -> Color -> [Move]
getFenMoves s c = getAllAllowedMoves c (getFenBoard s)

debugFen :: String -> CompMove
debugFen s = getBestMove (alpha0 Black) (beta0 Black) depth (getFenBoard s) Black

dsd :: CompMove
dsd = getBestMove (9999) (-9999) 3 (getFenBoard "1k6/8/8/8/8/2p5/8/6K1") Black

dsd1 :: CompMove
dsd1 = getBestMove (9999) (1.1) 2 (getFenBoard "1k6/8/8/8/8/8/2p5/6K1") White

dsd2 :: CompMove
dsd2 = getBestMove (1.1) 9999 1 (getFenBoard "1k6/8/8/8/8/8/2p5/5K2") Black --1.1

def :: CompMove
def = getBestMove (9999) (-9999) 3 (getFenBoard "1k6/ppp5/8/7r/8/8/K7/6RQ") Black

def1 :: CompMove
def1 = getBestMove (-9999) (-11.9) 2 (getFenBoard "1k6/ppp5/8/r7/8/8/K7/6RQ") White

def2 :: CompMove
def2 = getBestMove (11.9) 10.9 1 (getFenBoard "1k6/ppp5/8/r6Q/8/8/K7/6R1") Black

def2a :: CompMove
def2a = getBestMove (alpha0 Black) (10.9) 1 (getFenBoard "1k6/ppp5/8/r2Q4/8/8/K7/6R1") Black

dsd3 :: CompMove
dsd3 = getBestMove (1.9) (3.2) 0 (getFenBoard "1k4R1/pp6/8/2p5/8/8/K7/7r") White
