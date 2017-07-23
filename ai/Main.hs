{-# LANGUAGE DoAndIfThenElse #-}
module Main 
where

import ChessBot
import System.Environment

-- compile with "ghc -o ChessBot ChessBot.hs Main.hs"
-- call with e.g. ChessBot "rnbkqbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBKQBNR" "w"

-- return: move: [source position][space][target position] where the positions are defined as they are in the chessboardjs library of HA2
-- example: a8 b7

main :: IO ()
main = do 
    args <- getArgs 
    if length args >= 2 then 
            let arg1 = head args 
                arg2 = head (tail args) in
            if validateFENString (arg1) && (arg2 == "w" || arg2 == "b") then
                putStrLn (botFindMove arg1 arg2)
            else putStrLn ""
    else putStrLn "" 
