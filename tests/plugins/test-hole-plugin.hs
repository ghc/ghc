{-# OPTIONS -fplugin=HoleFitPlugin
            -fplugin-opt=HoleFitPlugin:4
            -funclutter-valid-hole-fits #-}
module Main where

import Prelude hiding (head, last)

import Data.List (head, last)


f, g, h, i, j :: [Int] -> Int
f = _too_long
j = _
i = _sort_by_mod_desc
g = _only_Data_List
h = _only_Prelude

main :: IO ()
main = return ()
