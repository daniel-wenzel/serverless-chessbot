-- Checks if a move does not leave the King in check.
{-# LANGUAGE OverloadedStrings #-}
module AllowedMoves where

import Board
import Color
import Move
import Plot
import PossibleMoves

--Returns all allowed moves
getAllAllowedMoves :: Color -> Board -> [Move]
getAllAllowedMoves c b = getAllAllowedMoves' c b (getAllMoves c b)

getAllAllowedMoves' :: Color -> Board -> [Move] -> [Move]
getAllAllowedMoves' _ _ [] = []
getAllAllowedMoves' c b (m:xs) =
    if (isInCheck c (getBoardAfterMove b m)) then
        getAllAllowedMoves' c b xs
    else [m] ++ getAllAllowedMoves' c b xs

--Returns all moves that result in the other player being checkmate. TODO: Check this method
getCheckMateMoves :: Color -> Board -> [Move]
getCheckMateMoves c b = getCheckMateMoves' c b (getAllAllowedMoves c b)

getCheckMateMoves' :: Color -> Board -> [Move] -> [Move]
getCheckMateMoves' _ _ [] = []
getCheckMateMoves' c b (m:xs) =
    if (isInCheckMate (getOppositeColor c) (getBoardAfterMove b m)) then
        [m] ++ getCheckMateMoves' c b xs
    else getCheckMateMoves' c b xs

--Returns true if the king of that color is under attack in that position
isInCheck :: Color -> Board -> Bool
isInCheck c b = isInCheck' (getKingPlot c b) (getAllMoves (getOppositeColor c) b)

isInCheck' :: Plot -> [Move] -> Bool
isInCheck' _ [] = False
isInCheck' (Plot xKing yKing c f) ((Move _ _ x y): xs) = (xKing == x && yKing == y) || isInCheck' (Plot xKing yKing c f) xs --fancy as fuck

--Returns true if the current player is checkmate (HE is the one who LOST)
isInCheckMate :: Color -> Board -> Bool
isInCheckMate c b = (isInCheck c b) && (null (getAllAllowedMoves c b))
