{-# OPTIONS -ftype-families #-}

module OverB
where
import OverA (C, D)

data instance C [Int] [a] = CListList2

type instance D [Int] [a] = Int