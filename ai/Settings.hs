{-# LANGUAGE OverloadedStrings #-}
module Settings where

import Color
depth :: Int
depth = 1

cutoff :: Bool
cutoff = True

alpha0 :: Color -> Float
alpha0 White = (-9999.0)
alpha0 Black = (9999.0)

beta0 :: Color -> Float
beta0 White = alpha0 Black
beta0 Black = alpha0 White

traceDepth :: Int
traceDepth = depth
