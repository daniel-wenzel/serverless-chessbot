{-# LANGUAGE OverloadedStrings #-}
module Color where

data Color = Black | White | None

instance Eq Color where
    (==) Black Black = True
    (==) White White = True
    (==) None None = True
    (==) _ _ = False

--Returns the opposite color (yay)
getOppositeColor :: Color -> Color
getOppositeColor Black = White
getOppositeColor White = Black
