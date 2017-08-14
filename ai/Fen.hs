{-# LANGUAGE OverloadedStrings #-}
module Fen where
import Data.Char
import Data.List.Split

startFEN :: String
startFEN = "rnbkqbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBKQBNR"

problemFen1 :: String
problemFen1 = "5Rnr/2k3pp/8/1BP2P2/p7/P7/3B2PP/R5K1"

-- an example of a board where one player is checkmate
checkMateFEN :: String
checkMateFEN = "3K3q/7r/8/8/8/8/7k/8"

-- checks if a FEN string really contains a valid chess board (8x8 fields)
validateFENString :: String -> Bool
validateFENString s = let rows = splitFENString s in
                      length rows == 8 && -- need 8 rows
                          foldr (((&&) . (== 8)) . countFigures) True rows -- 8 columns per row

  -- counts the fields of a string (a row)
countFigures :: String -> Int
countFigures [] = 0
countFigures s = let c = head s in
                   if isDigit c then digitToInt c + countFigures (tail s)
                   else
                      if c=='r' || c=='R' || c=='b' || c=='B' || c=='n' || c=='N' || c=='q' || c=='Q' || c=='k' || c=='K' || c=='p' || c=='P' then
                          1 + countFigures (tail s)
                      else
                          9 --more than eight means bad input

-- splits a string at the "/" -> converts a FEN string field to a list of FEN string rows
splitFENString :: String -> [String]
splitFENString = splitOn "/"
