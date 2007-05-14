{-# OPTIONS -ftype-families #-}

module OverB
where
import OverA (C)

data instance C [Int] [a] = CListList2
