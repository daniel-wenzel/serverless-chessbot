{-# LANGUAGE OverloadedStrings #-}
module CompMove where

import Data.Char
import Move
data CompMove = CompMove Int Int Int Int Float


instance Show CompMove where
      show (CompMove x1 y1 x2 y2 f) = [chr (97+x1)]++show (y1+1)++" "++[chr (97+x2)]++show (y2+1) ++"With Score: "++show f

instance Eq CompMove where
      (==) (CompMove _ _ _ _ f1) (CompMove _ _ _ _ f2) = f1 == f2

instance Ord CompMove where
      (CompMove _ _ _ _ f1) `compare` (CompMove _ _ _ _ f2) = f1  `compare` f2

getScore :: CompMove -> Float
getScore (CompMove _ _ _ _ f) = f


compMoveToMove :: CompMove -> Move
compMoveToMove (CompMove p q x y _) = Move p q x y
