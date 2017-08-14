{-# LANGUAGE OverloadedStrings #-}
module Piece where

data Piece = King | Pawn | Rook | Bishop | Queen | Knight | Empty

instance Eq Piece where
    (==) King King = True
    (==) Pawn Pawn = True
    (==) Rook Rook  = True
    (==) Bishop Bishop  = True
    (==) Queen Queen= True
    (==) Knight Knight  = True
    (==) Empty Empty  = True
    (==) _ _ = False


instance Show Piece where
  show King = "King"
  show Pawn = "Pawn"
  show Rook = "Rook"
  show Bishop = "Bishop"
  show Queen = "Queen"
  show Knight = "Knight"
  show Empty = "Empty"


getPieceValue :: Piece -> Float
getPieceValue Pawn = 1
getPieceValue Bishop = 3
getPieceValue Knight = 3
getPieceValue Rook = 5
getPieceValue Queen = 9
getPieceValue King = 1000
getPieceValue _ = 0
