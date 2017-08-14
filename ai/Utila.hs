{-# LANGUAGE OverloadedStrings #-}
module Utila where


indent :: Int -> String
indent 5 = " "
indent r = (indent (r+1))++" "
