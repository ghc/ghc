{-# LANGUAGE TypeFamilies #-}

module OverIndirectThisModB
where
import OverIndirectThisModA (C, D)

data instance C [Int] [a] = CListList2

type instance D [Int] [a] = Int
