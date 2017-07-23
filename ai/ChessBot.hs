    {-# LANGUAGE OverloadedStrings #-}
module ChessBot where

import Data.Char
import Data.List.Split

-------------------------------------------
--    Example Boards and Board validation
-------------------------------------------

-- this is the start board layout
startFEN :: String
startFEN = "rnbkqbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBKQBNR"

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

-----------------------------------------
--    Bot logic
-----------------------------------------

-- returns a list of all valid moves for the given board and color
-- parameter 1: first part of FEN String
-- parameter 2: "w"/"b" 
botListAllMoves :: String -> String -> [String]
--botListAllMoves board color = "a7 b6":"a7 c5":[] -- TODO: replace this with the real implementation
botListAllMoves board color = if color == "w" then map show (getFenMoves board White) else map show (getFenMoves board Black)

-- returns one valid move in the format [source target]
-- parameter 1: first part of FEN String
-- parameter 2: "w"/"b" 
botFindMove :: String -> String -> String
botFindMove board color = let b = (getFenBoard board) in if color == "w" then show (botFindMove' White b) else show (botFindMove' Black b)

botFindMove' :: Color -> Board -> Move
botFindMove' color board = let matemoves = (getCheckMateMoves color board)
                               allMoves =  (getAllAllowedMoves color board) in if length matemoves > 0 
                                                                            then head matemoves 
                                                                            else if length allMoves > 0
                                                                            then head allMoves
                                                                            else (Move 0 0 0 0) -- you are checkmate


-----------------------------------------
--    data information
-----------------------------------------
testBoard :: Board
testBoard = getFenBoard "1k6/8/8/8/7n/7r/3b4/7K"

data Color = Black | White | None
data Piece = King | Pawn | Rook | Bishop | Queen | Knight | Empty --TODO add figures here
data Plot = Plot Int Int Color Piece
type Board = [Plot]

--From x, from y, to x, to y
data Move = Move Int Int Int Int 


instance Eq Color where
    (==) Black Black = True 
    (==) White White = True
    (==) None None = True
    (==) _ _ = False

instance Eq Piece where
    (==) King King = True 
    (==) Pawn Pawn = True
    (==) Rook Rook  = True
    (==) Bishop Bishop  = True
    (==) Queen Queen= True
    (==) Knight Knight  = True
    (==) Empty Empty  = True
    (==) _ _ = False

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

instance Show Move where
    show (Move x1 y1 x2 y2) = [chr (97+x1)]++show (y1+1)++" "++[chr (97+x2)]++show (y2+1)


-----------------------------------------
--    handling FEN-String
-----------------------------------------

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

getFenMoves :: String -> Color -> [Move]
getFenMoves s c = getAllAllowedMoves c (getFenBoard s)  

getFenBoard :: String -> Board
getFenBoard s = loadFen 63 emptyBoard s 


-----------------------------------------
--    Functions
-----------------------------------------

emptyBoard :: Board
emptyBoard = emptyBoard' 63

emptyBoard' :: Int -> Board
emptyBoard' 0 = [Plot 0 0 None Empty]
emptyBoard' x = [Plot (x `mod` 8) (quot x 8)  None Empty] ++ emptyBoard' (x-1)

-- Returns the plot at the argument coordinates
getPlotAt :: Int -> Int -> Board -> Plot
getPlotAt x y ((Plot x1 y1 c d):xs) = if (x == x1 && y == y1) then (Plot x1 y1 c d) else getPlotAt x y xs 
getPlotAt x y _ = error ("Invalid plot coordinates: "++show x ++" "++show y)

-- Returns a plot at the coordinates, if they are on the board
getValidPlot :: Int -> Int -> Board -> [Plot]
getValidPlot x y b= if (x < 0 || y < 0 || x > 7 || y > 7) then [] else [getPlotAt x y b]

-- Returns all plots within a list of plots that are not hold by the argument color
getPlotsWithNotSameColor :: Color -> [Plot] -> [Plot]
getPlotsWithNotSameColor c [] = []
getPlotsWithNotSameColor c ((Plot x y c2 p):xs) = 
    if (c /= c2) then 
        [(Plot x y c2 p)] ++ getPlotsWithNotSameColor c xs 
    else getPlotsWithNotSameColor c xs

--Returns all moves. This includes moves that leave the King attacked.
getAllMoves :: Color -> Board -> [Move]
getAllMoves c b = getAllMoves' c b b

--This includes moves that leave the King attacked.
getAllMoves' :: Color -> Board -> Board -> [Move]
getAllMoves' c board [] = []
getAllMoves' c board (p: xs) = getPlotMoves p c board ++ getAllMoves' c board xs 


--Returns all moves that are possible from a given plot. Returns an empty list if there is not a figure with the color on the plot
getPlotMoves :: Plot -> Color -> Board-> [Move] 
getPlotMoves (Plot x y plot_color f) color b = 
    if (color == plot_color) then 
        getMoves (Plot x y plot_color f) (canMoveToPlots (Plot x y plot_color f) b) 
    else []

--Returns a list of plots you can go to from a certain plot. Doesnt care about check. Can (and often will be) []
getMoves :: Plot -> [Plot] -> [Move]
getMoves p [] = []
getMoves (Plot x1 y1 c p) ((Plot x2 y2 _ _):xs) = [(Move x1 y1 x2 y2)] ++ getMoves (Plot x1 y1 c p) xs

--Returns the opposite color (yay)
getOppositeColor :: Color -> Color
getOppositeColor Black = White
getOppositeColor White = Black

--Returns the plot on which the King with that color currently resides
getKingPlot :: Color -> Board -> Plot
getKingPlot c ((Plot x y plot_color King):xs) = if (c == plot_color)  then (Plot x y plot_color King) else getKingPlot c xs
getKingPlot c (_:xs) = getKingPlot c xs

--Returns true if the king of that color is under attack in that position
isInCheck :: Color -> Board -> Bool
isInCheck c b = isInCheck' (getKingPlot c b) (getAllMoves (getOppositeColor c) b)

isInCheck' :: Plot -> [Move] -> Bool
isInCheck' _ [] = False
isInCheck' (Plot xKing yKing c f) ((Move _ _ x y): xs) = (xKing == x && yKing == y) || isInCheck' (Plot xKing yKing c f) xs --fancy as fuck

--Returns true if the current player is checkmate (HE is the one who LOST)
isInCheckMate :: Color -> Board -> Bool
isInCheckMate c b = (isInCheck c b) && (null (getAllAllowedMoves c b)) 

--Returns a new Board with the plot changed (it will change the plot on the coordinates to the argument values)
setPlot :: Board -> Plot -> Board
setPlot ((Plot x1 y1 c1 f1): xs) (Plot x2 y2 c2 f2) = 
    if (x1 == x2 && y1 == y2) then 
        [(Plot x2 y2 c2 f2)] ++ xs--setPlot xs (Plot x2 y2 c2 f2)
    else     
        [(Plot x1 y1 c1 f1)] ++ setPlot xs (Plot x2 y2 c2 f2)

--Sets the coordinates of a plot
changePlotCoords :: Int -> Int -> Plot -> Plot
changePlotCoords x y (Plot _ _ c f) = Plot x y c f

--Returns how the board looks after a move

--Returns how the board looks after a move
getBoardAfterMove :: Board -> Move -> Board
getBoardAfterMove board (Move x1 y1 x2 y2) =let newBoard = setPlot (setPlot board (changePlotCoords x2 y2 (getPlotAt x1 y1 board))) (Plot x1 y1 None Empty) 
                                                newPlot = getPlotAt x2 y2 newBoard in
                                            if  y2 == 0 && getFigureAtPlot newPlot == Pawn && getColorOfPlot newPlot == Black -- Promotion Black
                                                then setPlot newBoard (Plot x2 y2 Black Queen)
                                            else if  y2 == 7 && getFigureAtPlot newPlot == Pawn && getColorOfPlot newPlot == White -- Promotion White
                                                then setPlot newBoard (Plot x2 y2 White Queen)                                    
                                                else newBoard

getColorOfPlot :: Plot -> Color
getColorOfPlot (Plot x y c f) = c 

getFigureAtPlot :: Plot -> Piece
getFigureAtPlot (Plot x y c f) = f 


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

    -- is the given plot empty?, color == none is empty
isEmpty :: Plot -> Bool
isEmpty (Plot _ _ c _)  = if c == None then True else False  

-- is there an opponent's piece on the plot?
getPlotsWithOppositeColor :: Color -> [Plot] -> [Plot]
getPlotsWithOppositeColor c [] = []
getPlotsWithOppositeColor c ((Plot x y c2 p):xs) = 
    if (c2 == (getOppositeColor c)) then 
        [(Plot x y c2 p)] ++ getPlotsWithOppositeColor c xs 
    else getPlotsWithOppositeColor c xs

-----------------------------------------
--    Figure Moves
-----------------------------------------


-- Checks where you can go to from a certain plot
canMoveToPlots :: Plot -> Board -> [Plot]
canMoveToPlots (Plot x y color King) board = getPlotsWithNotSameColor color (getNeighbours (Plot x y color King) board)
canMoveToPlots (Plot x y color Pawn) board = getPlotsWithNotSameColor color (getPawnPlots (Plot x y color Pawn) board)
canMoveToPlots (Plot x y color Rook) board = (getMovesInDir (Plot x y color Rook) (Dir 1 0) board) ++ (getMovesInDir (Plot x y color Rook) (Dir (-1) 0) board) 
                                            ++ (getMovesInDir (Plot x y color Rook) (Dir 0 (-1)) board) ++ (getMovesInDir (Plot x y color Rook) (Dir 0 1) board)
canMoveToPlots (Plot x y color Bishop) board = (getMovesInDir (Plot x y color Bishop) (Dir 1 1) board) ++ (getMovesInDir (Plot x y color Bishop) (Dir (-1) 1) board) 
                                            ++ (getMovesInDir (Plot x y color Bishop) (Dir 1 (-1)) board) ++ (getMovesInDir (Plot x y color Bishop) (Dir (-1) (-1)) board)
canMoveToPlots (Plot x y color Queen) board = canMoveToPlots (Plot x y color Bishop) board ++ canMoveToPlots (Plot x y color Rook) board                                            
canMoveToPlots (Plot x y color Knight) board = getPlotsWithNotSameColor color (getKnightPlots (Plot x y color Knight) board)
canMoveToPlots (Plot x y color _) board = error ("No idea how to move figure at "++show x ++" "++show y)

-- gets the neighbours of one plot (relevant for King)
getNeighbours :: Plot -> Board -> [Plot]
getNeighbours (Plot x y _ _) xs = getValidPlot (x-1) (y-1) xs ++ getValidPlot (x) (y-1) xs ++ getValidPlot (x+1) (y-1) xs ++
                                 getValidPlot (x-1) (y) xs ++               getValidPlot (x+1) (y) xs ++
                                 getValidPlot (x-1) (y+1) xs ++ getValidPlot (x) (y+1) xs ++ getValidPlot (x+1) (y+1) xs

-- returns all posibble moves for the Knight on the specified Plot
getKnightPlots :: Plot -> Board -> [Plot]
getKnightPlots (Plot x y _ _) xs = filter (\(Plot xP yP _ _) -> (abs (xP-x)) == 2 && (abs (yP-y)) == 1 || (abs (xP-x)) == 1 && (abs (yP-y)) == 2)  xs

-- returns all posibble moves for the Pawn on the specified Plot
getPawnPlots :: Plot -> Board -> [Plot]
getPawnPlots (Plot x y c p) b = (if c == White
                                                then getWhitePawnPlots (Plot x y c p) b 
                                                else getBlackPawnPlots (Plot x y c p) b)
--  returns all posibble moves for the black  Pawn on the specified Plot
getBlackPawnPlots :: Plot -> Board -> [Plot]
getBlackPawnPlots (Plot x y c _) b = let p = getValidPlot x (y-1) b in --laufen
                                        filter isEmpty (if (y == 6) && (isEmpty (head p)) then getValidPlot x (y-2) b ++ p
                                                else p) -- laufen
                                                 ++  getPlotsWithOppositeColor c (getValidPlot (x-1) (y-1) b ++ getValidPlot (x+1) (y-1) b)

--  returns all posibble moves for the white  Pawn on the specified Plot
getWhitePawnPlots :: Plot -> Board -> [Plot]
getWhitePawnPlots (Plot x y c _) b = let p = getValidPlot x (y+1) b in --laufen
                                        filter isEmpty (if (y == 1) && (isEmpty (head p)) then getValidPlot x (y+2) b ++ p
                                                else p) -- laufen
                                                 ++  getPlotsWithOppositeColor c (getValidPlot (x+1) (y+1) b ++ getValidPlot (x-1) (y+1) b)                                    

--getPlotInDir :: Plot -> Dir -> Board -> [Plot]
--getPlotInDir (Plot x y plot_c f):xs (Dir xdir ydir) b = getValidPlot x+xdir y+ydir b
data Dir = Dir  Int Int
getMovesInDir :: Plot -> Dir -> Board -> [Plot]
getMovesInDir (Plot x y c f) (Dir xdir ydir) b = getMovesInDir' c (getValidPlot (x+xdir) (y+ydir) b) (Dir xdir ydir) b

getMovesInDir' :: Color -> [Plot] -> Dir -> Board -> [Plot]
getMovesInDir' _ [] _ _ = []
getMovesInDir' col ((Plot x y plot_c f):xs) (Dir xdir ydir) b = 
    if (col == plot_c) then 
        [] 
    else if (plot_c == None) then 
        [(Plot x y plot_c f)] ++ getMovesInDir' col (getValidPlot (x+xdir) (y+ydir) b) (Dir xdir ydir) b
         else 
            [(Plot x y plot_c f)]


              


