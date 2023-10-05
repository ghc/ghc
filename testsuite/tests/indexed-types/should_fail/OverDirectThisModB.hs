{-# LANGUAGE TypeFamilies #-}

module OverDirectThisModB
where
import OverDirectThisModA (C, D)

data instance C [Int] [a] = CListList2

type instance D [Int] [a] = Int
